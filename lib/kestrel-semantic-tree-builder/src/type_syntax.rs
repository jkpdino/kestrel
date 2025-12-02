//! Shared type-from-syntax resolution utilities
//!
//! This module provides a unified interface for resolving types from syntax nodes.
//! It consolidates the duplicate logic that existed across:
//! - `resolvers/function.rs` (extract_type_from_ty_node, resolve_type_from_ty_node)
//! - `resolvers/field.rs` (resolve_field_type_from_syntax)
//! - `body_resolver.rs` (resolve_type_from_syntax)

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::ty::Ty;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::SymbolId;

use crate::queries::{Db, TypePathResolution};
use crate::utils::get_node_span;

/// Context for type resolution from syntax during the bind phase
pub struct TypeSyntaxContext<'a> {
    /// Database for resolving type paths
    pub db: &'a dyn Db,
    /// Diagnostics for error reporting
    pub diagnostics: &'a mut DiagnosticContext,
    /// File ID for error reporting
    pub file_id: usize,
    /// Source code for span calculation
    pub source: &'a str,
    /// Symbol ID of the context for resolution
    pub context_id: SymbolId,
}

/// Extract a type from a Ty syntax node without resolution (placeholder types).
///
/// This is used during the build phase when we don't have access to the database.
/// Type paths are returned as error types - they will be resolved during bind phase.
pub fn extract_type_from_ty_node(ty_node: &SyntaxNode, source: &str) -> Ty {
    let ty_span = get_node_span(ty_node, source);

    // Try TyPath
    if let Some(ty_path_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyPath)
    {
        if let Some(path_node) = ty_path_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Path)
        {
            let segments: Vec<String> = extract_path_segments(&path_node);
            if !segments.is_empty() {
                // Return error as placeholder - will be resolved during bind
                return Ty::error(ty_span);
            }
        }
    }

    // Try TyUnit
    if ty_node
        .children()
        .any(|child| child.kind() == SyntaxKind::TyUnit)
    {
        return Ty::unit(ty_span);
    }

    // Try TyNever
    if ty_node
        .children()
        .any(|child| child.kind() == SyntaxKind::TyNever)
    {
        return Ty::never(ty_span);
    }

    // Try TyFunction
    if let Some(fn_ty_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyFunction)
    {
        let mut param_types = Vec::new();
        if let Some(ty_list) = fn_ty_node
            .children()
            .find(|child| child.kind() == SyntaxKind::TyList)
        {
            for param_ty_node in ty_list.children().filter(|c| c.kind() == SyntaxKind::Ty) {
                param_types.push(extract_type_from_ty_node(&param_ty_node, source));
            }
        }

        let return_ty = fn_ty_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .last()
            .map(|ty| extract_type_from_ty_node(&ty, source))
            .unwrap_or_else(|| Ty::unit(ty_span.clone()));

        return Ty::function(param_types, return_ty, ty_span);
    }

    // Try TyTuple
    if let Some(tuple_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyTuple)
    {
        let element_types: Vec<Ty> = tuple_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .map(|ty| extract_type_from_ty_node(&ty, source))
            .collect();

        return Ty::tuple(element_types, ty_span);
    }

    // Fallback: error type
    Ty::error(ty_span)
}

/// Resolve a type from a Ty syntax node during bind phase.
///
/// This resolves type paths immediately using the database.
/// Emits diagnostics for resolution errors.
pub fn resolve_type_from_ty_node(ty_node: &SyntaxNode, ctx: &mut TypeSyntaxContext) -> Ty {
    let ty_span = get_node_span(ty_node, ctx.source);

    // Try TyPath
    if let Some(ty_path_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyPath)
    {
        if let Some(path_node) = ty_path_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Path)
        {
            let segments = extract_path_segments(&path_node);

            if !segments.is_empty() {
                return resolve_type_path(&segments, ty_span, ctx);
            }
        }
    }

    // Try TyUnit
    if ty_node.children().any(|child| child.kind() == SyntaxKind::TyUnit) {
        return Ty::unit(ty_span);
    }

    // Try TyNever
    if ty_node.children().any(|child| child.kind() == SyntaxKind::TyNever) {
        return Ty::never(ty_span);
    }

    // Try TyFunction - recursively resolve nested types
    if let Some(fn_ty_node) = ty_node.children().find(|child| child.kind() == SyntaxKind::TyFunction) {
        let mut param_types = Vec::new();
        if let Some(ty_list) = fn_ty_node.children().find(|child| child.kind() == SyntaxKind::TyList) {
            for param_ty_node in ty_list.children().filter(|c| c.kind() == SyntaxKind::Ty) {
                param_types.push(resolve_type_from_ty_node(&param_ty_node, ctx));
            }
        }

        let return_ty = fn_ty_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .last()
            .map(|ty| resolve_type_from_ty_node(&ty, ctx))
            .unwrap_or_else(|| Ty::unit(ty_span.clone()));

        return Ty::function(param_types, return_ty, ty_span);
    }

    // Try TyTuple - recursively resolve nested types
    if let Some(tuple_node) = ty_node.children().find(|child| child.kind() == SyntaxKind::TyTuple) {
        let element_types: Vec<Ty> = tuple_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .map(|ty| resolve_type_from_ty_node(&ty, ctx))
            .collect();

        return Ty::tuple(element_types, ty_span);
    }

    // Fallback: error type
    Ty::error(ty_span)
}

/// Extract path segments from a Path syntax node
fn extract_path_segments(path_node: &SyntaxNode) -> Vec<String> {
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

/// Resolve a type path and emit diagnostics on failure
fn resolve_type_path(
    segments: &[String],
    ty_span: kestrel_span::Span,
    ctx: &mut TypeSyntaxContext,
) -> Ty {
    match ctx.db.resolve_type_path(segments.to_vec(), ctx.context_id) {
        TypePathResolution::Resolved(resolved_ty) => resolved_ty,
        TypePathResolution::NotFound { segment, .. } => {
            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(format!("cannot find type '{}' in this scope", segment))
                .with_labels(vec![kestrel_reporting::Label::primary(ctx.file_id, ty_span.clone())
                    .with_message("not found")]);
            ctx.diagnostics.add_diagnostic(diagnostic);
            Ty::error(ty_span)
        }
        TypePathResolution::Ambiguous { segment, candidates, .. } => {
            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(format!("type '{}' is ambiguous ({} candidates)", segment, candidates.len()))
                .with_labels(vec![kestrel_reporting::Label::primary(ctx.file_id, ty_span.clone())
                    .with_message("ambiguous")]);
            ctx.diagnostics.add_diagnostic(diagnostic);
            Ty::error(ty_span)
        }
        TypePathResolution::NotAType { .. } => {
            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(format!("'{}' is not a type", segments.join(".")))
                .with_labels(vec![kestrel_reporting::Label::primary(ctx.file_id, ty_span.clone())
                    .with_message("not a type")]);
            ctx.diagnostics.add_diagnostic(diagnostic);
            Ty::error(ty_span)
        }
    }
}

/// Extract type from a node that contains a Ty child (without resolution)
pub fn extract_type_from_node(node: &SyntaxNode, source: &str) -> Ty {
    if let Some(ty_node) = node.children().find(|c| c.kind() == SyntaxKind::Ty) {
        return extract_type_from_ty_node(&ty_node, source);
    }
    Ty::error(0..0)
}

/// Resolve type from a node that contains a Ty child
pub fn resolve_type_from_node(node: &SyntaxNode, ctx: &mut TypeSyntaxContext) -> Ty {
    if let Some(ty_node) = node.children().find(|c| c.kind() == SyntaxKind::Ty) {
        return resolve_type_from_ty_node(&ty_node, ctx);
    }
    Ty::error(0..0)
}
