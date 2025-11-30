use std::sync::Arc;

use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::protocol::ProtocolSymbol;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::Resolver;
use crate::resolvers::type_parameter::{extract_type_parameters, extract_where_clause};
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope, get_node_span,
    get_visibility_span, parse_visibility,
};

/// Resolver for protocol declarations
pub struct ProtocolResolver;

impl Resolver for ProtocolResolver {
    fn build_declaration(
        &self,
        syntax: &SyntaxNode,
        source: &str,
        parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        // Extract name
        let name_str = extract_name(syntax)?;
        let name_node = find_child(syntax, SyntaxKind::Name)?;
        let name_span = get_node_span(&name_node, source);

        // Get full span
        let full_span = get_node_span(syntax, source);

        // Extract visibility
        let visibility_str = extract_visibility(syntax);
        let visibility_enum = visibility_str.as_deref().and_then(parse_visibility);

        let visibility_span = get_visibility_span(syntax, source).unwrap_or(name_span.clone());

        // Determine visibility scope
        let visibility_scope = find_visibility_scope(visibility_enum.as_ref(), parent, root);

        // Create visibility behavior
        let visibility_behavior =
            VisibilityBehavior::new(visibility_enum, visibility_span, visibility_scope);

        // Create the name object
        let name = Spanned::new(name_str, name_span);

        // Extract type parameters (will be empty if not a generic protocol)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Create the protocol symbol with type parameters and where clause
        let protocol_symbol = ProtocolSymbol::with_generics(
            name,
            full_span.clone(),
            visibility_behavior,
            type_parameters,
            where_clause,
            parent.cloned(),
        );
        let protocol_arc = Arc::new(protocol_symbol);

        let protocol_type = Ty::protocol(protocol_arc.clone(), full_span.clone());
        let typed_behavior = TypedBehavior::new(protocol_type, full_span.clone());

        protocol_arc.metadata().add_behavior(typed_behavior);

        let protocol_arc_dyn = protocol_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&protocol_arc_dyn);
        }

        Some(protocol_arc)
    }
}
