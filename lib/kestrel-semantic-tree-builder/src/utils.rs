use std::sync::Arc;

use kestrel_semantic_tree::behavior::visibility::Visibility;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

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

/// Check if a SyntaxKind is a declaration type
pub fn is_declaration(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::ClassDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ModuleDeclaration
    )
}

/// Get the span of a syntax node
///
/// NOTE: Rowan's text_range() can be incorrect when the lexer skips tokens (like comments).
/// This is a known limitation. For now, we use text_range() as-is.
/// A proper fix would require tracking original spans through the parser.
pub fn get_node_span(node: &SyntaxNode, _source: &str) -> Span {
    let text_range = node.text_range();
    text_range.start().into()..text_range.end().into()
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
