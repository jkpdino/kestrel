use std::sync::Arc;

use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::Resolver;
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause, extract_conformances};
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope, get_node_span,
    get_visibility_span, parse_visibility,
};

/// Resolver for struct declarations
pub struct StructResolver;

impl Resolver for StructResolver {
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

        // Extract type parameters (they'll have struct as parent later)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Extract conformances (protocols this struct conforms to)
        let conformances = extract_conformances(syntax, source);

        // Create the struct symbol with type parameters, where clause, and conformances
        let struct_symbol = StructSymbol::with_generics(
            name,
            full_span.clone(),
            visibility_behavior,
            type_parameters.clone(),
            where_clause,
            conformances,
            parent.cloned(),
        );
        let struct_arc = Arc::new(struct_symbol);

        let struct_type = Ty::r#struct(struct_arc.clone(), full_span.clone());
        let typed_behavior = TypedBehavior::new(struct_type, full_span.clone());

        struct_arc.metadata().add_behavior(typed_behavior);

        let struct_arc_dyn = struct_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add type parameters as children of the struct (not the module)
        // This ensures type parameters are in scope during type resolution
        add_type_params_as_children(&type_parameters, &struct_arc_dyn);

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&struct_arc_dyn);
        }

        Some(struct_arc)
    }
}
