use kestrel_semantic_tree::ty::{Ty, TyKind};
use semantic_tree::symbol::SymbolId;

use crate::queries::{self, Db, TypePathResolution};

/// Context for type resolution during the binding phase
pub struct TypeResolutionContext<'a> {
    pub db: &'a dyn Db,
}

/// Recursively resolve all Path variants in a type
///
/// This function walks through the type structure and replaces all `TyKind::Path`
/// variants with their resolved types using scope-aware name resolution.
///
/// # Arguments
/// * `ty` - The type to resolve
/// * `ctx` - Type resolution context containing the database
/// * `context_id` - The symbol ID of the resolution context (for scope/visibility)
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
    context_id: SymbolId,
) -> Option<Ty> {
    match ty.kind() {
        // Base types that don't need resolution
        TyKind::Unit
        | TyKind::Never
        | TyKind::Int(_)
        | TyKind::Float(_)
        | TyKind::Bool
        | TyKind::String => Some(ty.clone()),

        // Path types need to be resolved using scope-aware resolution
        TyKind::Path(segments) => {
            match queries::resolve_type_path(ctx.db, segments.clone(), context_id) {
                TypePathResolution::Resolved(resolved_ty) => Some(resolved_ty),
                _ => None, // NotFound, Ambiguous, or NotAType
            }
        }

        // Class types are already resolved
        TyKind::Class(_) => Some(ty.clone()),

        // Struct types are already resolved
        TyKind::Struct(_) => Some(ty.clone()),

        // Protocol types are already resolved
        TyKind::Protocol(_) => Some(ty.clone()),

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
                .map(|elem_ty| resolve_type(elem_ty, ctx, context_id))
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
                .map(|param_ty| resolve_type(param_ty, ctx, context_id))
                .collect();

            // Resolve return type
            let resolved_return = resolve_type(return_type, ctx, context_id);

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
    use std::sync::Arc;
    use std::collections::HashMap;
    use kestrel_semantic_tree::error::ModuleNotFoundError;
    use kestrel_semantic_tree::language::KestrelLanguage;
    use semantic_tree::symbol::Symbol;
    use crate::queries::{Scope, Import, SymbolResolution};

    /// A mock Db for testing that doesn't require a full semantic tree.
    /// Only used for testing basic type resolution (unit, never, tuple, function).
    struct MockDb {
        root_id: SymbolId,
    }

    impl MockDb {
        fn new() -> Self {
            MockDb {
                root_id: SymbolId::new(),
            }
        }
    }

    impl Db for MockDb {
        fn symbol_by_id(&self, _id: SymbolId) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
            None // Not needed for basic type tests
        }

        fn scope_for(&self, symbol_id: SymbolId) -> Arc<Scope> {
            Arc::new(Scope {
                symbol_id,
                imports: HashMap::new(),
                declarations: HashMap::new(),
                parent: None,
            })
        }

        fn resolve_name(&self, _name: String, _context: SymbolId) -> SymbolResolution {
            SymbolResolution::NotFound
        }

        fn imports_in_scope(&self, _symbol_id: SymbolId) -> Vec<Arc<Import>> {
            vec![]
        }

        fn is_visible_from(&self, _target: SymbolId, _context: SymbolId) -> bool {
            true
        }

        fn resolve_module_path(
            &self,
            path: Vec<String>,
            _context: SymbolId,
        ) -> Result<SymbolId, ModuleNotFoundError> {
            Err(ModuleNotFoundError {
                path,
                failed_segment_index: 0,
                path_span: 0..0,
                failed_segment_span: 0..0,
            })
        }

        fn resolve_type_path(&self, path: Vec<String>, _context: SymbolId) -> TypePathResolution {
            TypePathResolution::NotFound {
                segment: path.first().cloned().unwrap_or_default(),
                index: 0,
            }
        }
    }

    #[test]
    fn test_resolve_unit_type() {
        let ty = Ty::unit(0..2);
        let db = MockDb::new();

        let ctx = TypeResolutionContext { db: &db };

        let resolved = resolve_type(&ty, &ctx, db.root_id);
        assert!(resolved.is_some());
        assert!(resolved.unwrap().is_unit());
    }

    #[test]
    fn test_resolve_never_type() {
        let ty = Ty::never(0..1);
        let db = MockDb::new();

        let ctx = TypeResolutionContext { db: &db };

        let resolved = resolve_type(&ty, &ctx, db.root_id);
        assert!(resolved.is_some());
        assert!(resolved.unwrap().is_never());
    }

    #[test]
    fn test_resolve_tuple_type() {
        let ty = Ty::tuple(vec![Ty::unit(0..2), Ty::never(4..5)], 0..6);
        let db = MockDb::new();

        let ctx = TypeResolutionContext { db: &db };

        let resolved = resolve_type(&ty, &ctx, db.root_id);
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
        let db = MockDb::new();

        let ctx = TypeResolutionContext { db: &db };

        let resolved = resolve_type(&ty, &ctx, db.root_id);
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
