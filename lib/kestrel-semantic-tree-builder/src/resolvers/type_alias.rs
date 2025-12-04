use std::sync::Arc;

use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::type_alias::{TypeAliasSymbol, TypeAliasTypedBehavior};
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause};
use crate::type_syntax::{resolve_type_from_ty_node, TypeSyntaxContext};
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope, get_node_span,
    get_visibility_span, parse_visibility,
};

/// Resolver for type alias declarations
pub struct TypeAliasResolver;

impl Resolver for TypeAliasResolver {
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

        // Use error type as placeholder - actual type will be resolved during bind from syntax
        let placeholder_type = Ty::error(full_span.clone());
        let syntactic_typed_behavior = TypedBehavior::new(placeholder_type, full_span.clone());

        // Extract type parameters (they'll have type alias as parent later)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Create the type alias symbol with type parameters and where clause
        let type_alias_symbol = TypeAliasSymbol::with_generics(
            name.clone(),
            full_span.clone(),
            visibility_behavior,
            syntactic_typed_behavior,
            type_parameters.clone(),
            where_clause,
            parent.cloned(),
        );
        let type_alias_arc = Arc::new(type_alias_symbol);

        // The type of a type alias symbol is TypeAlias (referring to itself)
        // This is what allows type checkers to distinguish type aliases from their underlying types
        let type_alias_type =
            kestrel_semantic_tree::ty::Ty::type_alias(type_alias_arc.clone(), full_span.clone());
        let semantic_typed_behavior = TypedBehavior::new(type_alias_type, full_span.clone());

        // Add a second TypedBehavior with the TypeAlias type
        // Now the symbol has two TypedBehaviors:
        // 1. The first one (from new()) contains the syntactic aliased type
        // 2. This one contains the TypeAlias type
        type_alias_arc
            .metadata()
            .add_behavior(semantic_typed_behavior);

        let type_alias_arc_dyn = type_alias_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add type parameters as children of the type alias (not the module)
        // This ensures type parameters are in scope during type resolution
        add_type_params_as_children(&type_parameters, &type_alias_arc_dyn);

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&type_alias_arc_dyn);
        }

        Some(type_alias_arc_dyn)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        syntax: &SyntaxNode,
        context: &mut BindingContext,
    ) {
        let symbol_id = symbol.metadata().id();

        // Enter the cycle detector to track that we're binding this type alias
        // This shouldn't fail on first entry, but we check anyway
        if context.type_alias_cycle_detector.enter(symbol_id).is_err() {
            // We're already binding this type alias - this shouldn't happen
            // in normal sequential binding, but guard against it
            return;
        }

        // Get file_id and source for this symbol
        let (file_id, source) = context.get_file_context(symbol);

        // Extract and resolve the aliased type from syntax using unified type resolution
        let resolved_type = resolve_aliased_type_from_syntax(syntax, &source, symbol_id, context, file_id);

        // Add the resolved type as a TypeAliasTypedBehavior
        let type_alias_typed_behavior = TypeAliasTypedBehavior::new(resolved_type);
        symbol.metadata().add_behavior(type_alias_typed_behavior);

        // Exit the cycle detector after we're done resolving this type alias
        context.type_alias_cycle_detector.exit();
    }
}

/// Resolve the aliased type from a TypeAliasDeclaration syntax node during bind phase.
///
/// Uses the unified type resolution from type_syntax module.
fn resolve_aliased_type_from_syntax(
    syntax: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Ty {
    // Find the AliasedType node
    let aliased_type_node = match find_child(syntax, SyntaxKind::AliasedType) {
        Some(node) => node,
        None => return Ty::error(get_node_span(syntax, source)),
    };

    // Try to find a Ty node first. If it doesn't exist, use the AliasedType node itself.
    let ty_node = find_child(&aliased_type_node, SyntaxKind::Ty)
        .unwrap_or_else(|| aliased_type_node.clone());

    // Use unified type resolution
    let mut type_ctx = TypeSyntaxContext::new(ctx.db, ctx.diagnostics, file_id, source, context_id);
    resolve_type_from_ty_node(&ty_node, &mut type_ctx)
}