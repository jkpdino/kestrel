use std::sync::Arc;

use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::r#fn::FnSymbol;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::Resolver;
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope,
    get_node_span, get_visibility_span, parse_visibility,
};

/// Resolver for function declarations
pub struct FnResolver;

impl Resolver for FnResolver {
    fn build_declaration(
        &self,
        syntax: &SyntaxNode,
        source: &str,
        parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        // Extract name from Name node
        let name_str = extract_name(syntax)?;
        let name_node = find_child(syntax, SyntaxKind::Name)?;
        let name_span = get_node_span(&name_node, source);
        let name = Spanned::new(name_str, name_span.clone());

        // Get full span of the declaration
        let full_span = get_node_span(syntax, source);

        // Extract visibility modifier
        let visibility_str = extract_visibility(syntax);
        let visibility_enum = visibility_str.as_deref().and_then(parse_visibility);
        let visibility_span = get_visibility_span(syntax, source).unwrap_or(name_span);

        // Determine visibility scope (what can access this)
        let visibility_scope = find_visibility_scope(visibility_enum.as_ref(), parent, root);

        // Create visibility behavior
        let visibility_behavior = VisibilityBehavior::new(
            visibility_enum,
            visibility_span,
            visibility_scope,
        );

        // Create the function symbol
        let fn_symbol = FnSymbol::new(name, full_span, visibility_behavior);
        let fn_arc: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(fn_symbol);

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&fn_arc);
        }

        Some(fn_arc)
    }
}
