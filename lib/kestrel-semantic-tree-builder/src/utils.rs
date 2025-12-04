use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::conformances::ConformancesBehavior;
use kestrel_semantic_tree::behavior::visibility::Visibility;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode, SyntaxElement};
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::diagnostics::{NotAProtocolContext, NotAProtocolError, UnresolvedTypeError};
use crate::queries::TypePathResolution;
use crate::resolver::BindingContext;

/// Find a child node with the specified kind
pub fn find_child(syntax: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    syntax.children().find(|n| n.kind() == kind)
}

/// Extract name from a Name node
pub fn extract_name(syntax: &SyntaxNode) -> Option<String> {
    let name_node = find_child(syntax, SyntaxKind::Name)?;

    name_node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tok| tok.kind() == SyntaxKind::Identifier)
        .map(|tok| tok.text().to_string())
}

/// Check if a SyntaxKind is trivia (whitespace or comment)
fn is_trivia(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::Whitespace | SyntaxKind::LineComment | SyntaxKind::BlockComment)
}

/// Extract visibility modifier from a node with a Visibility child
pub fn extract_visibility(syntax: &SyntaxNode) -> Option<String> {
    let visibility_node = find_child(syntax, SyntaxKind::Visibility)?;

    // Skip trivia tokens
    let visibility_token = visibility_node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tok| !is_trivia(tok.kind()))?;

    let vis_text = match visibility_token.kind() {
        SyntaxKind::Public => "public",
        SyntaxKind::Private => "private",
        SyntaxKind::Internal => "internal",
        SyntaxKind::Fileprivate => "fileprivate",
        _ => return None,
    };

    Some(vis_text.to_string())
}


/// Check if a SyntaxKind is trivia (whitespace or comment)
fn is_trivia_kind(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::Whitespace | SyntaxKind::LineComment | SyntaxKind::BlockComment)
}

/// Get the span of a syntax node, excluding leading trivia
///
/// This function computes the span of a syntax node by finding the first
/// non-trivia token and using its start position as the span start.
/// The end position is taken from the full text_range.
pub fn get_node_span(node: &SyntaxNode, _source: &str) -> Span {
    let text_range = node.text_range();
    let end: usize = text_range.end().into();

    // Find the first non-trivia element to get the real start position
    let start = find_first_non_trivia_start(node)
        .unwrap_or_else(|| text_range.start().into());

    start..end
}

/// Recursively find the start position of the first non-trivia token in a node
fn find_first_non_trivia_start(node: &SyntaxNode) -> Option<usize> {
    for child in node.children_with_tokens() {
        match child {
            SyntaxElement::Token(t) => {
                if !is_trivia_kind(t.kind()) {
                    return Some(t.text_range().start().into());
                }
            }
            SyntaxElement::Node(n) => {
                // Recursively check child nodes
                if let Some(start) = find_first_non_trivia_start(&n) {
                    return Some(start);
                }
            }
        }
    }
    None
}

/// Parse visibility string to Visibility enum
pub fn parse_visibility(vis_str: &str) -> Option<Visibility> {
    match vis_str {
        "public" => Some(Visibility::Public),
        "private" => Some(Visibility::Private),
        "internal" => Some(Visibility::Internal),
        "fileprivate" => Some(Visibility::Fileprivate),
        _ => None,
    }
}

/// Get the span of the visibility node
pub fn get_visibility_span(syntax: &SyntaxNode, source: &str) -> Option<Span> {
    let visibility_node = find_child(syntax, SyntaxKind::Visibility)?;
    Some(get_node_span(&visibility_node, source))
}

/// Find an ancestor symbol of the specified kind by walking up the parent chain.
///
/// Returns the first ancestor matching the kind, or None if no such ancestor exists.
pub fn find_ancestor_of_kind(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    kind: KestrelSymbolKind,
) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
    let mut current = Some(symbol.clone());

    while let Some(s) = current {
        if s.metadata().kind() == kind {
            return Some(s);
        }
        current = s.metadata().parent();
    }

    None
}

/// Find the scope symbol where this visibility level is accessible
/// - Public/Internal: root symbol (module-level visibility)
/// - Fileprivate: SourceFile symbol (file-level visibility)
/// - Private: immediate container (parent)
/// - None (no visibility specified): defaults to internal, so root
pub fn find_visibility_scope(
    visibility: Option<&Visibility>,
    parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
    root: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Arc<dyn Symbol<KestrelLanguage>> {
    match visibility {
        Some(Visibility::Private) => {
            // Private is visible only in the immediate container
            parent.cloned().unwrap_or_else(|| root.clone())
        }
        Some(Visibility::Fileprivate) => {
            // Fileprivate is visible within the same SourceFile
            // Walk up from parent to find the containing SourceFile
            parent
                .and_then(|p| find_ancestor_of_kind(p, KestrelSymbolKind::SourceFile))
                .unwrap_or_else(|| root.clone())
        }
        Some(Visibility::Internal) | Some(Visibility::Public) | None => {
            // Public and Internal are visible at root level
            root.clone()
        }
    }
}

/// Information about a symbol's source file
pub struct SourceFileInfo {
    /// The file ID for diagnostics
    pub file_id: usize,
    /// The file name
    pub file_name: String,
}

/// Get source file info for a symbol by walking up to its SourceFile parent.
///
/// Returns the file_id and file_name in a single parent-chain traversal.
pub fn get_source_file_info(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &DiagnosticContext,
) -> Option<SourceFileInfo> {
    let mut current = symbol.clone();
    loop {
        if current.metadata().kind() == KestrelSymbolKind::SourceFile {
            let file_name = current.metadata().name().value.clone();
            let file_id = diagnostics.get_file_id(&file_name)?;
            return Some(SourceFileInfo { file_id, file_name });
        }
        match current.metadata().parent() {
            Some(parent) => current = parent,
            None => return None,
        }
    }
}

/// Get the file_id for a symbol by walking up to its SourceFile parent
pub fn get_file_id_for_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &DiagnosticContext,
) -> usize {
    get_source_file_info(symbol, diagnostics)
        .map(|info| info.file_id)
        .unwrap_or(0)
}

/// Extract path segments from a Path syntax node
pub fn extract_path_segments(path_node: &SyntaxNode) -> Vec<String> {
    path_node
        .children()
        .filter(|child| child.kind() == SyntaxKind::PathElement)
        .filter_map(|path_elem| {
            path_elem
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|tok| tok.kind() == SyntaxKind::Identifier)
                .map(|tok| tok.text().to_string())
        })
        .collect()
}

/// Extract identifier text from a Name syntax node
pub fn extract_identifier_from_name(name_node: &SyntaxNode) -> Option<String> {
    name_node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tok| tok.kind() == SyntaxKind::Identifier)
        .map(|tok| tok.text().to_string())
}

/// Resolve conformances/inheritance from syntax and add them as a ConformancesBehavior.
///
/// This is a shared function used by both struct (for conformances) and protocol (for inheritance).
/// The `context` parameter determines the error context used in diagnostics.
pub fn resolve_conformance_list(
    syntax: &SyntaxNode,
    source: &str,
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    context_id: SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
    error_context: NotAProtocolContext,
) {
    // Find the ConformanceList node
    let conformance_list = match find_child(syntax, SyntaxKind::ConformanceList) {
        Some(node) => node,
        None => return,
    };

    let mut resolved = Vec::new();

    // Process each ConformanceItem
    for item in conformance_list.children() {
        if item.kind() != SyntaxKind::ConformanceItem {
            continue;
        }

        // ConformanceItem contains a Ty node
        let ty_node = match find_child(&item, SyntaxKind::Ty) {
            Some(node) => node,
            None => continue,
        };

        let span = get_node_span(&ty_node, source);

        // Find the TyPath inside Ty
        let ty_path = match ty_node.children().find(|c| c.kind() == SyntaxKind::TyPath) {
            Some(node) => node,
            None => continue,
        };

        // Extract path from TyPath
        let path_node = match find_child(&ty_path, SyntaxKind::Path) {
            Some(node) => node,
            None => continue,
        };

        // Extract path segments
        let segments = extract_path_segments(&path_node);

        if segments.is_empty() {
            continue;
        }

        let type_name = segments.join(".");

        // Resolve the path
        match ctx.db.resolve_type_path(segments.clone(), context_id) {
            TypePathResolution::Resolved(resolved_ty) => {
                match resolved_ty.kind() {
                    TyKind::Protocol { .. } => {
                        // Valid protocol - add to resolved list
                        resolved.push(resolved_ty);
                    }
                    TyKind::Struct { symbol, .. } => {
                        ctx.diagnostics.throw(NotAProtocolError {
                            span: span.clone(),
                            name: symbol.metadata().name().value.clone(),
                            context: error_context,
                        }, file_id);
                        resolved.push(Ty::error(span));
                    }
                    _ => {
                        ctx.diagnostics.throw(NotAProtocolError {
                            span: span.clone(),
                            name: type_name.clone(),
                            context: error_context,
                        }, file_id);
                        resolved.push(Ty::error(span));
                    }
                }
            }
            TypePathResolution::NotFound { .. } => {
                ctx.diagnostics.throw(UnresolvedTypeError {
                    span: span.clone(),
                    type_name: type_name.clone(),
                }, file_id);
                resolved.push(Ty::error(span));
            }
            TypePathResolution::Ambiguous { .. } | TypePathResolution::NotAType { .. } => {
                ctx.diagnostics.throw(NotAProtocolError {
                    span: span.clone(),
                    name: type_name.clone(),
                    context: error_context,
                }, file_id);
                resolved.push(Ty::error(span));
            }
        }
    }

    // Add ConformancesBehavior with resolved conformances
    let conformances_behavior = ConformancesBehavior::new(resolved);
    symbol.metadata().add_behavior(conformances_behavior);
}
