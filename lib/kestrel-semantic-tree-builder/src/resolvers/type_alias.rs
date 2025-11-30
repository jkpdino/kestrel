use std::sync::Arc;

use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::error::{CircularTypeAliasError, CycleParticipant};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::type_alias::{TypeAliasSymbol, TypeAliasTypedBehavior};
use kestrel_semantic_tree::ty::TyKind;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{extract_type_parameters, extract_where_clause};
use crate::type_resolver::{resolve_type, TypeResolutionContext};
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

        // Extract type parameters (will be empty if not a generic type alias)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Create the type alias symbol with type parameters and where clause
        let type_alias_symbol = TypeAliasSymbol::with_generics(
            name.clone(),
            full_span.clone(),
            visibility_behavior,
            syntactic_typed_behavior,
            type_parameters,
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

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&type_alias_arc_dyn);
        }

        Some(type_alias_arc_dyn)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        context: &mut BindingContext,
    ) {
        let symbol_id = symbol.metadata().id();

        // Enter the cycle detector to track that we're binding this type alias
        // This shouldn't fail on first entry, but we check anyway
        if let Err(_) = context.type_alias_cycle_detector.enter(symbol_id) {
            // We're already binding this type alias - this shouldn't happen
            // in normal sequential binding, but guard against it
            return;
        }

        // Extract the original aliased type from the first TypedBehavior
        // (The one created in build_declaration with the syntactic type)
        let behaviors = symbol.metadata().behaviors();

        // Find the first TypedBehavior - this contains the syntactic aliased type
        let typed_behavior = behaviors
            .iter()
            .find(|b| matches!(b.kind(), KestrelBehaviorKind::Typed))
            .and_then(|b| b.as_ref().downcast_ref::<TypedBehavior>());

        let Some(typed_behavior) = typed_behavior else {
            // No TypedBehavior found - this shouldn't happen
            context.type_alias_cycle_detector.exit();
            return;
        };

        let syntactic_type = typed_behavior.ty();

        // Create type resolution context with the Db
        let type_ctx = TypeResolutionContext { db: context.db };

        // Resolve all Path variants in the type using scope-aware resolution
        match resolve_type(syntactic_type, &type_ctx, symbol_id) {
            Some(resolved_type) => {
                // Check if the resolved type contains a type alias that would create a cycle
                if let Some(cycle_error) =
                    check_resolved_type_for_cycles(&resolved_type, symbol, context)
                {
                    context.diagnostics.throw(cycle_error, context.file_id);
                } else {
                    // Add the resolved type as a TypeAliasTypedBehavior
                    let type_alias_typed_behavior = TypeAliasTypedBehavior::new(resolved_type);
                    symbol.metadata().add_behavior(type_alias_typed_behavior);
                }
            }
            None => {
                // Type resolution failed - the error will be emitted by resolve_type_path
                // For now, we don't add a TypeAliasTypedBehavior
            }
        }

        // Exit the cycle detector after we're done resolving this type alias
        context.type_alias_cycle_detector.exit();
    }
}

/// Check if a resolved type contains a type alias that would create a cycle
/// This detects direct self-references (type A = A) and checks the cycle detector
/// for any type aliases that are currently being resolved (indirect cycles).
fn check_resolved_type_for_cycles(
    ty: &kestrel_semantic_tree::ty::Ty,
    current_symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    context: &mut BindingContext,
) -> Option<CircularTypeAliasError> {
    check_type_for_cycles_recursive(ty, current_symbol, context)
}

fn check_type_for_cycles_recursive(
    ty: &kestrel_semantic_tree::ty::Ty,
    current_symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    context: &mut BindingContext,
) -> Option<CircularTypeAliasError> {
    match ty.kind() {
        TyKind::TypeAlias { symbol: alias_symbol, .. } => {
            let alias_id = alias_symbol.metadata().id();
            let current_id = current_symbol.metadata().id();

            // Check for direct self-reference (type A = A)
            if alias_id == current_id {
                let origin = CycleParticipant {
                    name: current_symbol.metadata().name().value.clone(),
                    name_span: current_symbol.metadata().name().span.clone(),
                    file_id: context.file_id_for_symbol(current_symbol),
                };

                return Some(CircularTypeAliasError {
                    origin,
                    cycle: vec![], // Self-cycle has no intermediate participants
                });
            }

            // Check if this type alias is currently being resolved (indirect cycle)
            // This would happen if: type A = B, type B = A
            // When binding A, we enter A in the detector
            // When resolving B in A's definition, if B were being bound, we'd detect it
            // But since B hasn't been bound yet, we can't detect A -> B -> A this way
            //
            // For now, we can only detect self-references. Detecting A -> B -> A requires
            // a post-binding pass that follows the type alias chains.
            if context.type_alias_cycle_detector.is_active(&alias_id) {
                let origin = CycleParticipant {
                    name: current_symbol.metadata().name().value.clone(),
                    name_span: current_symbol.metadata().name().span.clone(),
                    file_id: context.file_id_for_symbol(current_symbol),
                };

                let cycle = vec![CycleParticipant {
                    name: alias_symbol.metadata().name().value.clone(),
                    name_span: alias_symbol.metadata().name().span.clone(),
                    file_id: context.file_id_for_symbol(
                        &(alias_symbol.clone() as Arc<dyn Symbol<KestrelLanguage>>),
                    ),
                }];

                return Some(CircularTypeAliasError { origin, cycle });
            }

            None
        }
        TyKind::Tuple(elements) => {
            for elem in elements {
                if let Some(err) =
                    check_type_for_cycles_recursive(elem, current_symbol, context)
                {
                    return Some(err);
                }
            }
            None
        }
        TyKind::Function {
            params,
            return_type,
        } => {
            for param in params {
                if let Some(err) =
                    check_type_for_cycles_recursive(param, current_symbol, context)
                {
                    return Some(err);
                }
            }
            check_type_for_cycles_recursive(return_type, current_symbol, context)
        }
        _ => None,
    }
}
