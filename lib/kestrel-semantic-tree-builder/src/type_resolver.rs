use std::sync::Arc;

use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use semantic_tree::symbol::{Symbol, SymbolTable};

use crate::path_resolver::resolve_type_path;

/// Context for type resolution during the binding phase
pub struct TypeResolutionContext<'a> {
    pub symbol_table: &'a SymbolTable<KestrelLanguage>,
    pub root: &'a Arc<dyn Symbol<KestrelLanguage>>,
}

/// Recursively resolve all Path variants in a type
///
/// This function walks through the type structure and replaces all `TyKind::Path`
/// variants with their resolved types by looking up the path in the symbol table.
///
/// # Arguments
/// * `ty` - The type to resolve
/// * `context` - Type resolution context containing symbol table and root
/// * `resolution_context` - The symbol context for visibility checking
///
/// # Returns
/// * `Some(Ty)` if resolution succeeds (all paths resolved)
/// * `None` if any path fails to resolve
///
/// # Circular Reference Detection
/// This function does not currently detect circular type aliases.
/// Future implementation should track the chain of type aliases being resolved
/// and detect cycles.
pub fn resolve_type(
    ty: &Ty,
    ctx: &TypeResolutionContext,
    resolution_context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Option<Ty> {
    match ty.kind() {
        // Base types that don't need resolution
        TyKind::Unit | TyKind::Never => Some(ty.clone()),

        // Path types need to be resolved
        TyKind::Path(segments) => {
            // Resolve the path to a type
            resolve_type_path(segments, ctx.symbol_table, resolution_context)
        }

        // Class types are already resolved
        TyKind::Class(_) => Some(ty.clone()),

        // Type alias types should be resolved to their underlying type
        // Note: This might need special handling to prevent infinite recursion
        TyKind::TypeAlias(_) => {
            // For now, just return the type alias as-is
            // Future: resolve to the underlying type
            Some(ty.clone())
        }

        // Tuple types: recursively resolve element types
        TyKind::Tuple(elements) => {
            let resolved_elements: Option<Vec<Ty>> = elements
                .iter()
                .map(|elem_ty| resolve_type(elem_ty, ctx, resolution_context))
                .collect();

            resolved_elements.map(|elements| Ty::tuple(elements, ty.span().clone()))
        }

        // Function types: recursively resolve parameter and return types
        TyKind::Function {
            params,
            return_type,
        } => {
            // Resolve all parameter types
            let resolved_params: Option<Vec<Ty>> = params
                .iter()
                .map(|param_ty| resolve_type(param_ty, ctx, resolution_context))
                .collect();

            // Resolve return type
            let resolved_return = resolve_type(return_type, ctx, resolution_context);

            // Combine results
            match (resolved_params, resolved_return) {
                (Some(params), Some(return_type)) => {
                    Some(Ty::function(params, return_type, ty.span().clone()))
                }
                _ => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
    use kestrel_span::Name;
    use semantic_tree::symbol::SymbolMetadataBuilder;

    fn create_test_root() -> Arc<dyn Symbol<KestrelLanguage>> {
        let root_name = Name::new("Root".to_string(), 0..4);
        let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::Class)
            .with_name(root_name)
            .with_declaration_span(0..4)
            .with_span(0..100)
            .build();

        #[derive(Debug)]
        struct TestRootSymbol {
            metadata: semantic_tree::symbol::SymbolMetadata<KestrelLanguage>,
        }

        impl Symbol<KestrelLanguage> for TestRootSymbol {
            fn metadata(&self) -> &semantic_tree::symbol::SymbolMetadata<KestrelLanguage> {
                &self.metadata
            }
        }

        Arc::new(TestRootSymbol { metadata })
    }

    #[test]
    fn test_resolve_unit_type() {
        let ty = Ty::unit(0..2);
        let symbol_table = SymbolTable::new();
        let root = create_test_root();

        let ctx = TypeResolutionContext {
            symbol_table: &symbol_table,
            root: &root,
        };

        let resolved = resolve_type(&ty, &ctx, &root);
        assert!(resolved.is_some());
        assert!(resolved.unwrap().is_unit());
    }

    #[test]
    fn test_resolve_never_type() {
        let ty = Ty::never(0..1);
        let symbol_table = SymbolTable::new();
        let root = create_test_root();

        let ctx = TypeResolutionContext {
            symbol_table: &symbol_table,
            root: &root,
        };

        let resolved = resolve_type(&ty, &ctx, &root);
        assert!(resolved.is_some());
        assert!(resolved.unwrap().is_never());
    }

    #[test]
    fn test_resolve_tuple_type() {
        let ty = Ty::tuple(vec![Ty::unit(0..2), Ty::never(4..5)], 0..6);
        let symbol_table = SymbolTable::new();
        let root = create_test_root();

        let ctx = TypeResolutionContext {
            symbol_table: &symbol_table,
            root: &root,
        };

        let resolved = resolve_type(&ty, &ctx, &root);
        assert!(resolved.is_some());
        let resolved_ty = resolved.unwrap();
        assert!(resolved_ty.is_tuple());

        let elements = resolved_ty.as_tuple().unwrap();
        assert_eq!(elements.len(), 2);
        assert!(elements[0].is_unit());
        assert!(elements[1].is_never());
    }

    #[test]
    fn test_resolve_function_type() {
        let ty = Ty::function(
            vec![Ty::unit(0..2), Ty::never(4..5)],
            Ty::unit(10..12),
            0..12,
        );
        let symbol_table = SymbolTable::new();
        let root = create_test_root();

        let ctx = TypeResolutionContext {
            symbol_table: &symbol_table,
            root: &root,
        };

        let resolved = resolve_type(&ty, &ctx, &root);
        assert!(resolved.is_some());
        let resolved_ty = resolved.unwrap();
        assert!(resolved_ty.is_function());

        let (params, return_type) = resolved_ty.as_function().unwrap();
        assert_eq!(params.len(), 2);
        assert!(params[0].is_unit());
        assert!(params[1].is_never());
        assert!(return_type.is_unit());
    }
}
