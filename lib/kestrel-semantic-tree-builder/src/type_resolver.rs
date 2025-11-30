use kestrel_reporting::{DiagnosticContext, IntoDiagnostic};
use kestrel_semantic_tree::ty::{Ty, TyKind};
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::diagnostics::{AmbiguousTypeError, NotATypeError, NotGenericError, TooFewTypeArgumentsError, TooManyTypeArgumentsError, UnresolvedTypeError};
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
        TyKind::Path(segments, type_args) => {
            match queries::resolve_type_path(ctx.db, segments.clone(), context_id) {
                TypePathResolution::Resolved(resolved_ty) => {
                    // If there are type arguments, we need to create substitutions
                    if type_args.is_empty() {
                        Some(resolved_ty)
                    } else {
                        // Resolve each type argument recursively
                        let resolved_args: Option<Vec<Ty>> = type_args
                            .iter()
                            .map(|arg| resolve_type(arg, ctx, context_id))
                            .collect();

                        match resolved_args {
                            Some(args) => {
                                // Apply type arguments, converting Result to Option
                                // Errors are logged but resolution continues
                                apply_type_arguments(&resolved_ty, args, ty.span().clone()).ok()
                            }
                            None => None,
                        }
                    }
                }
                _ => None, // NotFound, Ambiguous, or NotAType
            }
        }

        // Type parameter types are already resolved
        TyKind::TypeParameter(_) => Some(ty.clone()),

        // Struct types are already resolved
        TyKind::Struct { .. } => Some(ty.clone()),

        // Protocol types are already resolved
        TyKind::Protocol { .. } => Some(ty.clone()),

        // Type alias types should be resolved to their underlying type
        // Note: This might need special handling to prevent infinite recursion
        TyKind::TypeAlias { .. } => {
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

        // Array types: recursively resolve element type
        TyKind::Array(element_type) => {
            resolve_type(element_type, ctx, context_id)
                .map(|resolved_elem| Ty::array(resolved_elem, ty.span().clone()))
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
    match ty.kind() {
        // Base types that don't need resolution
        TyKind::Unit
        | TyKind::Never
        | TyKind::Int(_)
        | TyKind::Float(_)
        | TyKind::Bool
        | TyKind::String => Some(ty.clone()),

        // Path types need resolution with error reporting
        TyKind::Path(segments, type_args) => {
            match queries::resolve_type_path(ctx.db, segments.clone(), context_id) {
                TypePathResolution::Resolved(resolved_ty) => {
                    if type_args.is_empty() {
                        Some(resolved_ty)
                    } else {
                        // Resolve each type argument, collecting all errors
                        let resolved_args: Option<Vec<Ty>> = type_args
                            .iter()
                            .map(|arg| resolve_type_with_diagnostics(arg, ctx, context_id, diagnostics, file_id))
                            .collect();

                        match resolved_args {
                            Some(args) => {
                                match apply_type_arguments(&resolved_ty, args, ty.span().clone()) {
                                    Ok(result_ty) => Some(result_ty),
                                    Err(err) => {
                                        report_type_argument_error(err, ty.span().clone(), segments, diagnostics, file_id);
                                        None
                                    }
                                }
                            }
                            None => None,
                        }
                    }
                }
                TypePathResolution::NotFound { segment, .. } => {
                    let error = UnresolvedTypeError {
                        span: ty.span().clone(),
                        type_name: segment,
                    };
                    diagnostics.add_diagnostic(error.into_diagnostic(file_id));
                    None
                }
                TypePathResolution::Ambiguous { segment, candidates, .. } => {
                    let error = AmbiguousTypeError {
                        span: ty.span().clone(),
                        type_name: segment,
                        candidate_count: candidates.len(),
                    };
                    diagnostics.add_diagnostic(error.into_diagnostic(file_id));
                    None
                }
                TypePathResolution::NotAType { symbol_id } => {
                    // Try to get the symbol's kind for a better error message
                    let (name, kind) = ctx.db.symbol_by_id(symbol_id)
                        .map(|s| {
                            let name = s.metadata().name().value.clone();
                            let kind = format!("{:?}", s.metadata().kind());
                            (name, kind)
                        })
                        .unwrap_or_else(|| (segments.join("."), "symbol".to_string()));

                    let error = NotATypeError {
                        span: ty.span().clone(),
                        name,
                        actual_kind: kind,
                    };
                    diagnostics.add_diagnostic(error.into_diagnostic(file_id));
                    None
                }
            }
        }

        // Type parameter types are already resolved
        TyKind::TypeParameter(_) => Some(ty.clone()),

        // Struct/Protocol/TypeAlias types are already resolved
        TyKind::Struct { .. } | TyKind::Protocol { .. } | TyKind::TypeAlias { .. } => {
            Some(ty.clone())
        }

        // Tuple types: recursively resolve element types, collecting all errors
        TyKind::Tuple(elements) => {
            let resolved_elements: Vec<Option<Ty>> = elements
                .iter()
                .map(|elem_ty| resolve_type_with_diagnostics(elem_ty, ctx, context_id, diagnostics, file_id))
                .collect();

            // Check if all resolved successfully
            if resolved_elements.iter().all(|e| e.is_some()) {
                let elements: Vec<Ty> = resolved_elements.into_iter().flatten().collect();
                Some(Ty::tuple(elements, ty.span().clone()))
            } else {
                None
            }
        }

        // Array types: recursively resolve element type
        TyKind::Array(element_type) => {
            resolve_type_with_diagnostics(element_type, ctx, context_id, diagnostics, file_id)
                .map(|resolved_elem| Ty::array(resolved_elem, ty.span().clone()))
        }

        // Function types: resolve params and return type, collecting all errors
        TyKind::Function { params, return_type } => {
            let resolved_params: Vec<Option<Ty>> = params
                .iter()
                .map(|param_ty| resolve_type_with_diagnostics(param_ty, ctx, context_id, diagnostics, file_id))
                .collect();

            let resolved_return = resolve_type_with_diagnostics(return_type, ctx, context_id, diagnostics, file_id);

            // Check if all resolved successfully
            if resolved_params.iter().all(|p| p.is_some()) && resolved_return.is_some() {
                let params: Vec<Ty> = resolved_params.into_iter().flatten().collect();
                Some(Ty::function(params, resolved_return.unwrap(), ty.span().clone()))
            } else {
                None
            }
        }
    }
}

/// Error when applying type arguments to a generic type.
///
/// This error provides detailed information for diagnostic messages.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeArgumentError {
    /// Too many type arguments provided
    TooManyArguments {
        type_name: String,
        expected: usize,
        got: usize,
    },
    /// Too few type arguments and missing ones don't have defaults
    TooFewArguments {
        type_name: String,
        expected: usize,
        got: usize,
        first_missing: String,
    },
    /// Type doesn't accept type arguments (not generic)
    NotGeneric { type_name: String },
    /// Type doesn't support type arguments (primitives, tuples, etc.)
    NotAGenericType,
}

/// Apply type arguments to a resolved type, creating a generic instantiation.
///
/// Given a type like `Ty::Struct { symbol: List, substitutions: {} }` and type arguments `[Int]`,
/// this creates `Ty::Struct { symbol: List, substitutions: { T -> Int } }` where T is List's
/// first type parameter.
///
/// Supports default type arguments: `Map[K, V = String]` can be instantiated as `Map[Int]`.
fn apply_type_arguments(
    resolved_ty: &Ty,
    type_args: Vec<Ty>,
    span: kestrel_span::Span,
) -> Result<Ty, TypeArgumentError> {
    use kestrel_semantic_tree::symbol::type_parameter::TypeParameterSymbol;
    use std::sync::Arc;

    /// Helper to build substitutions from type parameters and arguments
    fn build_substitutions(
        type_params: &[Arc<TypeParameterSymbol>],
        type_args: Vec<Ty>,
        type_name: &str,
        existing_subs: &kestrel_semantic_tree::ty::Substitutions,
    ) -> Result<kestrel_semantic_tree::ty::Substitutions, TypeArgumentError> {
        let num_args = type_args.len();
        let num_params = type_params.len();

        // Too many arguments is always an error
        if num_args > num_params {
            return Err(TypeArgumentError::TooManyArguments {
                type_name: type_name.to_string(),
                expected: num_params,
                got: num_args,
            });
        }

        // Count required parameters (those without defaults)
        let required_params = type_params.iter().take_while(|p| !p.has_default()).count();

        // Too few arguments is an error if missing params don't have defaults
        if num_args < required_params {
            let first_missing = type_params
                .get(num_args)
                .map(|p| p.metadata().name().value.clone())
                .unwrap_or_else(|| "?".to_string());
            return Err(TypeArgumentError::TooFewArguments {
                type_name: type_name.to_string(),
                expected: required_params,
                got: num_args,
                first_missing,
            });
        }

        // Build substitutions
        let mut new_subs = existing_subs.clone();
        for (i, param) in type_params.iter().enumerate() {
            let param_id = Symbol::metadata(param.as_ref()).id();
            if i < num_args {
                // Use provided argument
                new_subs.insert(param_id, type_args[i].clone());
            } else if let Some(default) = param.default() {
                // Use default type
                new_subs.insert(param_id, default.clone());
            }
            // If we reach here without a default, we already errored above
        }

        Ok(new_subs)
    }

    match resolved_ty.kind() {
        TyKind::Struct { symbol, substitutions } => {
            let type_params = symbol.type_parameters();
            let type_name = symbol.metadata().name().value.clone();

            // Non-generic struct with type args is an error
            if type_params.is_empty() && !type_args.is_empty() {
                return Err(TypeArgumentError::NotGeneric { type_name });
            }

            let new_subs = build_substitutions(type_params, type_args, &type_name, substitutions)?;
            Ok(Ty::generic_struct(symbol.clone(), new_subs, span))
        }

        TyKind::Protocol { symbol, substitutions } => {
            let type_params = symbol.type_parameters();
            let type_name = symbol.metadata().name().value.clone();

            if type_params.is_empty() && !type_args.is_empty() {
                return Err(TypeArgumentError::NotGeneric { type_name });
            }

            let new_subs = build_substitutions(type_params, type_args, &type_name, substitutions)?;
            Ok(Ty::generic_protocol(symbol.clone(), new_subs, span))
        }

        TyKind::TypeAlias { symbol, substitutions } => {
            let type_params = symbol.type_parameters();
            let type_name = symbol.metadata().name().value.clone();

            if type_params.is_empty() && !type_args.is_empty() {
                return Err(TypeArgumentError::NotGeneric { type_name });
            }

            let new_subs = build_substitutions(type_params, type_args, &type_name, substitutions)?;
            Ok(Ty::generic_type_alias(symbol.clone(), new_subs, span))
        }

        // Type parameters with type arguments (e.g., T[Int]) are not supported
        TyKind::TypeParameter(_) => Err(TypeArgumentError::NotAGenericType),

        // Base types don't accept type arguments
        TyKind::Unit
        | TyKind::Never
        | TyKind::Int(_)
        | TyKind::Float(_)
        | TyKind::Bool
        | TyKind::String
        | TyKind::Tuple(_)
        | TyKind::Array(_)
        | TyKind::Function { .. }
        | TyKind::Path(_, _) => Err(TypeArgumentError::NotAGenericType),
    }
}

/// Report a type argument error to the diagnostic context.
fn report_type_argument_error(
    error: TypeArgumentError,
    span: kestrel_span::Span,
    segments: &[String],
    diagnostics: &mut DiagnosticContext,
    file_id: usize,
) {
    match error {
        TypeArgumentError::TooManyArguments { type_name, expected, got } => {
            let err = TooManyTypeArgumentsError {
                span,
                type_name,
                expected,
                got,
            };
            diagnostics.add_diagnostic(err.into_diagnostic(file_id));
        }
        TypeArgumentError::TooFewArguments { type_name, expected, got, first_missing } => {
            let err = TooFewTypeArgumentsError {
                span,
                type_name,
                expected,
                got,
                first_missing,
            };
            diagnostics.add_diagnostic(err.into_diagnostic(file_id));
        }
        TypeArgumentError::NotGeneric { type_name } => {
            let err = NotGenericError {
                span,
                type_name,
            };
            diagnostics.add_diagnostic(err.into_diagnostic(file_id));
        }
        TypeArgumentError::NotAGenericType => {
            // For primitives like Int[String], use the path segments as the type name
            let type_name = segments.join(".");
            let err = NotGenericError {
                span,
                type_name,
            };
            diagnostics.add_diagnostic(err.into_diagnostic(file_id));
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
