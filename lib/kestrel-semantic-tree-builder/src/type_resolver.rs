use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use semantic_tree::symbol::SymbolId;

/// Context for type resolution during the binding phase
pub struct TypeResolutionContext<'a> {
    pub db: &'a dyn crate::queries::Db,
}

/// Internal context for type resolution that optionally collects diagnostics
struct ResolveContext<'a> {
    diagnostics: Option<(&'a mut DiagnosticContext, usize)>, // (diagnostics, file_id)
}

impl<'a> ResolveContext<'a> {
    fn without_diagnostics() -> Self {
        Self { diagnostics: None }
    }

    fn with_diagnostics(diagnostics: &'a mut DiagnosticContext, file_id: usize) -> Self {
        Self { diagnostics: Some((diagnostics, file_id)) }
    }
}

/// Internal implementation of type resolution that handles both variants
fn resolve_type_impl(
    ty: &Ty,
    _ctx: &TypeResolutionContext,
    _context_id: SymbolId,
    resolve_ctx: &mut ResolveContext,
) -> Option<Ty> {
    match ty.kind() {
        // Base types and special types that don't need resolution
        TyKind::Unit
        | TyKind::Never
        | TyKind::Int(_)
        | TyKind::Float(_)
        | TyKind::Bool
        | TyKind::String
        | TyKind::Error
        | TyKind::SelfType
        | TyKind::Inferred => Some(ty.clone()),

        // Type parameter types are already resolved
        TyKind::TypeParameter(_) => Some(ty.clone()),

        // Struct/Protocol/TypeAlias types are already resolved
        TyKind::Struct { .. } | TyKind::Protocol { .. } | TyKind::TypeAlias { .. } => {
            Some(ty.clone())
        }

        // Tuple types: recursively resolve element types
        TyKind::Tuple(elements) => {
            let resolved_elements: Vec<Option<Ty>> = elements
                .iter()
                .map(|elem_ty| resolve_type_impl(elem_ty, _ctx, _context_id, resolve_ctx))
                .collect();

            // With diagnostics: continue even if some fail to collect all errors
            // Without diagnostics: short-circuit on first failure
            if resolve_ctx.diagnostics.is_some() {
                if resolved_elements.iter().all(|e| e.is_some()) {
                    let elements: Vec<Ty> = resolved_elements.into_iter().flatten().collect();
                    Some(Ty::tuple(elements, ty.span().clone()))
                } else {
                    None
                }
            } else {
                let collected: Option<Vec<Ty>> = resolved_elements.into_iter().collect();
                collected.map(|elements| Ty::tuple(elements, ty.span().clone()))
            }
        }

        // Array types: recursively resolve element type
        TyKind::Array(element_type) => {
            resolve_type_impl(element_type, _ctx, _context_id, resolve_ctx)
                .map(|resolved_elem| Ty::array(resolved_elem, ty.span().clone()))
        }

        // Function types: recursively resolve parameter and return types
        TyKind::Function { params, return_type } => {
            let resolved_params: Vec<Option<Ty>> = params
                .iter()
                .map(|param_ty| resolve_type_impl(param_ty, _ctx, _context_id, resolve_ctx))
                .collect();

            let resolved_return = resolve_type_impl(return_type, _ctx, _context_id, resolve_ctx);

            // With diagnostics: continue even if some fail to collect all errors
            // Without diagnostics: short-circuit on first failure
            if resolve_ctx.diagnostics.is_some() {
                if resolved_params.iter().all(|p| p.is_some()) && resolved_return.is_some() {
                    let params: Vec<Ty> = resolved_params.into_iter().flatten().collect();
                    Some(Ty::function(params, resolved_return.unwrap(), ty.span().clone()))
                } else {
                    None
                }
            } else {
                let collected_params: Option<Vec<Ty>> = resolved_params.into_iter().collect();
                match (collected_params, resolved_return) {
                    (Some(params), Some(return_type)) => {
                        Some(Ty::function(params, return_type, ty.span().clone()))
                    }
                    _ => None,
                }
            }
        }
    }
}

/// Recursively resolve nested types
///
/// This function walks through the type structure and ensures all nested types
/// (like in tuples, arrays, functions) are properly resolved.
///
/// # Arguments
/// * `ty` - The type to resolve
/// * `ctx` - Type resolution context containing the database
/// * `context_id` - The symbol ID of the resolution context (for scope/visibility)
///
/// # Returns
/// * `Some(Ty)` if resolution succeeds (all paths resolved)
/// * `None` if any path fails to resolve
pub fn resolve_type(
    ty: &Ty,
    ctx: &TypeResolutionContext,
    context_id: SymbolId,
) -> Option<Ty> {
    let mut resolve_ctx = ResolveContext::without_diagnostics();
    resolve_type_impl(ty, ctx, context_id, &mut resolve_ctx)
}

/// Resolve a type with diagnostic reporting.
///
/// Unlike `resolve_type`, this function reports detailed errors to a diagnostic context
/// when type resolution fails, providing better error messages to users.
///
/// # Arguments
/// * `ty` - The type to resolve
/// * `ctx` - Type resolution context containing the database
/// * `context_id` - The symbol ID of the resolution context
/// * `diagnostics` - Diagnostic context to report errors to
/// * `file_id` - File ID for error reporting
///
/// # Returns
/// * `Some(Ty)` if resolution succeeds
/// * `None` if resolution fails (errors are reported to diagnostics)
pub fn resolve_type_with_diagnostics(
    ty: &Ty,
    ctx: &TypeResolutionContext,
    context_id: SymbolId,
    diagnostics: &mut DiagnosticContext,
    file_id: usize,
) -> Option<Ty> {
    let mut resolve_ctx = ResolveContext::with_diagnostics(diagnostics, file_id);
    resolve_type_impl(ty, ctx, context_id, &mut resolve_ctx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::collections::HashMap;
    use kestrel_semantic_tree::error::ModuleNotFoundError;
    use kestrel_semantic_tree::language::KestrelLanguage;
    use semantic_tree::symbol::Symbol;
    use crate::queries::{Db, Scope, Import, SymbolResolution, TypePathResolution, ValuePathResolution};

    /// A mock Db for testing that doesn't require a full semantic tree.
    struct MockDb {
        root_id: SymbolId,
    }

    impl MockDb {
        fn new() -> Self {
            MockDb { root_id: SymbolId::new() }
        }
    }

    impl Db for MockDb {
        fn symbol_by_id(&self, _id: SymbolId) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
            None
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

        fn resolve_value_path(&self, path: Vec<String>, _context: SymbolId) -> ValuePathResolution {
            ValuePathResolution::NotFound {
                segment: path.first().cloned().unwrap_or_default(),
                index: 0,
            }
        }

        fn visible_children_from(&self, _parent: SymbolId, _context: SymbolId) -> Vec<Arc<dyn Symbol<KestrelLanguage>>> {
            vec![]
        }

        fn find_child_by_name(&self, _parent: SymbolId, _name: &str) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
            None
        }
    }

    fn make_ctx(db: &MockDb) -> TypeResolutionContext {
        TypeResolutionContext { db }
    }

    #[test]
    fn test_resolve_unit_type() {
        let db = MockDb::new();
        let resolved = resolve_type(&Ty::unit(0..2), &make_ctx(&db), db.root_id);
        assert!(resolved.is_some());
        assert!(resolved.unwrap().is_unit());
    }

    #[test]
    fn test_resolve_never_type() {
        let db = MockDb::new();
        let resolved = resolve_type(&Ty::never(0..1), &make_ctx(&db), db.root_id);
        assert!(resolved.is_some());
        assert!(resolved.unwrap().is_never());
    }

    #[test]
    fn test_resolve_tuple_type() {
        let db = MockDb::new();
        let ty = Ty::tuple(vec![Ty::unit(0..2), Ty::never(4..5)], 0..6);
        let resolved = resolve_type(&ty, &make_ctx(&db), db.root_id);

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
        let db = MockDb::new();
        let ty = Ty::function(
            vec![Ty::unit(0..2), Ty::never(4..5)],
            Ty::unit(10..12),
            0..12,
        );
        let resolved = resolve_type(&ty, &make_ctx(&db), db.root_id);

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
