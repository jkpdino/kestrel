use std::sync::Arc;

use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::type_alias::TypeAliasSymbol;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::{BindingContext, Resolver};
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

        // Extract the aliased type from AliasedType node
        let aliased_type_node = find_child(syntax, SyntaxKind::AliasedType)?;

        // Try to find a Ty node first. If it doesn't exist, use the AliasedType node itself.
        // This handles both the expected structure (AliasedType -> Ty -> ...)
        // and the current parser output (AliasedType -> Identifier)
        let ty_node = find_child(&aliased_type_node, SyntaxKind::Ty)
            .unwrap_or_else(|| aliased_type_node.clone());

        // Build the semantic type using TypeBuilder
        // This will contain the syntactic type (may have unresolved Path variants)
        let aliased_type = crate::type_builder::TypeBuilder::build(&ty_node, source)?;

        // Store the syntactic aliased type in the first TypedBehavior
        // This will be used during the binding phase to resolve Path variants
        let syntactic_typed_behavior = TypedBehavior::new(aliased_type.clone(), full_span.clone());

        // Create the type alias symbol
        let type_alias_symbol = TypeAliasSymbol::new(
            name.clone(),
            full_span.clone(),
            visibility_behavior,
            syntactic_typed_behavior,
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

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&type_alias_arc_dyn);
        }

        Some(type_alias_arc_dyn)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        _context: &mut BindingContext,
    ) {
        // TODO: Migrate type resolution to query-based API
        // For now, type alias resolution is disabled until path_resolver is updated
        // to use the Db trait instead of SymbolTable

        // Extract the original aliased type from the first TypedBehavior
        // (The one created in build_declaration with the syntactic type)
        let behaviors = symbol.metadata().behaviors();

        // Find the first TypedBehavior - this contains the syntactic aliased type
        let typed_behavior = behaviors
            .iter()
            .find(|b| matches!(b.kind(), KestrelBehaviorKind::Typed))
            .and_then(|b| b.as_ref().downcast_ref::<TypedBehavior>());

        let Some(_typed_behavior) = typed_behavior else {
            // No TypedBehavior found - this shouldn't happen
            return;
        };

        // TODO: When path_resolver is updated to use Db trait:
        // let aliased_type = typed_behavior.ty();
        // let resolved_type = resolve_type_with_db(aliased_type, context.db, symbol);
        // if let Some(resolved_type) = resolved_type {
        //     let type_alias_typed_behavior = TypeAliasTypedBehavior::new(resolved_type);
        //     symbol.metadata().add_behavior(type_alias_typed_behavior);
        // }
    }
}
