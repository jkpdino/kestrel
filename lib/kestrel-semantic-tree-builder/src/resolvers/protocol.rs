use std::sync::Arc;

use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::protocol::ProtocolSymbol;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::diagnostics::NotAProtocolContext;
use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause};
use crate::syntax::{
    extract_name, extract_visibility, find_child, find_visibility_scope,
    get_node_span, get_visibility_span, parse_visibility, resolve_conformance_list,
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

        // Extract type parameters (they'll have protocol as parent later)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Create the protocol symbol with type parameters and where clause
        // Inherited protocols are resolved and added as a ConformancesBehavior during bind phase
        let protocol_symbol = ProtocolSymbol::with_generics(
            name,
            full_span.clone(),
            visibility_behavior,
            type_parameters.clone(),
            where_clause,
            parent.cloned(),
        );
        let protocol_arc = Arc::new(protocol_symbol);

        let protocol_type = Ty::protocol(protocol_arc.clone(), full_span.clone());
        let typed_behavior = TypedBehavior::new(protocol_type, full_span.clone());

        protocol_arc.metadata().add_behavior(typed_behavior);

        let protocol_arc_dyn = protocol_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add type parameters as children of the protocol (not the module)
        // This ensures type parameters are in scope during type resolution
        add_type_params_as_children(&type_parameters, &protocol_arc_dyn);

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&protocol_arc_dyn);
        }

        Some(protocol_arc)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        syntax: &SyntaxNode,
        context: &mut BindingContext,
    ) {
        // Only process protocol symbols
        if symbol.metadata().kind() != KestrelSymbolKind::Protocol {
            return;
        }

        let symbol_id = symbol.metadata().id();

        // Get file_id and source for this symbol
        let (file_id, source) = context.get_file_context(symbol);

        // Resolve inherited protocols from syntax and add as behavior
        resolve_conformance_list(
            syntax,
            &source,
            symbol,
            symbol_id,
            context,
            file_id,
            NotAProtocolContext::Inheritance,
        );
    }
}

