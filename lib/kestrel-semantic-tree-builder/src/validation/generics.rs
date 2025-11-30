//! Validation pass for generics
//!
//! Validates generic type declarations and usages:
//! - Duplicate type parameter names
//! - Default ordering (defaults must come after non-defaults)
//! - Type arity (correct number of type arguments)
//! - Non-generic type with args error
//! - Bounds reference valid protocols (future)

use std::collections::HashMap;
use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::error::{DefaultOrderingError, DuplicateTypeParameterError, NonProtocolBoundError};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::function::FunctionSymbol;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::protocol::ProtocolSymbol;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::symbol::type_alias::TypeAliasSymbol;
use kestrel_semantic_tree::symbol::type_parameter::TypeParameterSymbol;
use kestrel_semantic_tree::ty::{Constraint, TyKind, WhereClause};
use kestrel_span::Span;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::queries::{Db, TypePathResolution};
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass for generics
pub struct GenericsPass;

impl GenericsPass {
    const NAME: &'static str = "generics";
}

impl ValidationPass for GenericsPass {
    fn name(&self) -> &'static str {
        Self::NAME
    }

    fn validate(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        _config: &ValidationConfig,
    ) {
        validate_symbol(root, db, diagnostics);
    }
}

/// Recursively validate symbols for generics issues
fn validate_symbol(symbol: &Arc<dyn Symbol<KestrelLanguage>>, db: &SemanticDatabase, diagnostics: &mut DiagnosticContext) {
    let kind = symbol.metadata().kind();

    // Dereference Arc to get the trait object for downcasting
    let symbol_ref: &dyn Symbol<KestrelLanguage> = symbol.as_ref();

    // Check type parameters on symbols that can have them
    match kind {
        KestrelSymbolKind::Struct => {
            if let Some(struct_sym) = symbol_ref.as_any().downcast_ref::<StructSymbol>() {
                validate_type_parameters(
                    struct_sym.type_parameters(),
                    symbol,
                    diagnostics,
                );
                validate_where_clause(
                    struct_sym.where_clause(),
                    symbol,
                    db,
                    diagnostics,
                );
            }
        }
        KestrelSymbolKind::Function => {
            if let Some(func_sym) = symbol_ref.as_any().downcast_ref::<FunctionSymbol>() {
                validate_type_parameters(
                    func_sym.type_parameters(),
                    symbol,
                    diagnostics,
                );
                validate_where_clause(
                    func_sym.where_clause(),
                    symbol,
                    db,
                    diagnostics,
                );
            }
        }
        KestrelSymbolKind::Protocol => {
            if let Some(proto_sym) = symbol_ref.as_any().downcast_ref::<ProtocolSymbol>() {
                validate_type_parameters(
                    proto_sym.type_parameters(),
                    symbol,
                    diagnostics,
                );
                validate_where_clause(
                    proto_sym.where_clause(),
                    symbol,
                    db,
                    diagnostics,
                );
            }
        }
        KestrelSymbolKind::TypeAlias => {
            if let Some(alias_sym) = symbol_ref.as_any().downcast_ref::<TypeAliasSymbol>() {
                validate_type_parameters(
                    alias_sym.type_parameters(),
                    symbol,
                    diagnostics,
                );
                validate_where_clause(
                    alias_sym.where_clause(),
                    symbol,
                    db,
                    diagnostics,
                );
            }
        }
        _ => {}
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, db, diagnostics);
    }
}

/// Validate a list of type parameters
fn validate_type_parameters(
    type_params: &[Arc<TypeParameterSymbol>],
    container: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
) {
    if type_params.is_empty() {
        return;
    }

    let file_id = get_file_id_for_symbol(container, diagnostics);

    // Check for duplicates
    check_duplicate_type_params(type_params, file_id, diagnostics);

    // Check default ordering
    check_default_ordering(type_params, file_id, diagnostics);
}

/// Check for duplicate type parameter names
fn check_duplicate_type_params(
    type_params: &[Arc<TypeParameterSymbol>],
    file_id: usize,
    diagnostics: &mut DiagnosticContext,
) {
    // Map from name to first occurrence span
    let mut seen: HashMap<String, Span> = HashMap::new();

    for param in type_params {
        let name = param.metadata().name().value.clone();
        let span = param.metadata().name().span.clone();

        if let Some(original_span) = seen.get(&name) {
            let error = DuplicateTypeParameterError {
                name: name.clone(),
                duplicate_span: span,
                original_span: original_span.clone(),
            };
            diagnostics.throw(error, file_id);
        } else {
            seen.insert(name, span);
        }
    }
}

/// Check that type parameters with defaults come after those without
fn check_default_ordering(
    type_params: &[Arc<TypeParameterSymbol>],
    file_id: usize,
    diagnostics: &mut DiagnosticContext,
) {
    // Find first parameter with a default
    let mut first_with_default: Option<&Arc<TypeParameterSymbol>> = None;

    for param in type_params {
        if param.default().is_some() {
            if first_with_default.is_none() {
                first_with_default = Some(param);
            }
        } else if let Some(prev_with_default) = first_with_default {
            // Found a param without default after one with default
            let error = DefaultOrderingError {
                param_with_default: prev_with_default.metadata().name().value.clone(),
                param_without_default: param.metadata().name().value.clone(),
                with_default_span: prev_with_default.metadata().name().span.clone(),
                without_default_span: param.metadata().name().span.clone(),
            };
            diagnostics.throw(error, file_id);
            // Only report once per ordering issue
            break;
        }
    }
}

/// Validate where clause constraints
fn validate_where_clause(
    where_clause: &WhereClause,
    container: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    if where_clause.is_empty() {
        return;
    }

    let file_id = get_file_id_for_symbol(container, diagnostics);
    let context_id = container.metadata().id();

    // Validate each constraint
    for constraint in &where_clause.constraints {
        validate_constraint(constraint, context_id, db, file_id, diagnostics);
    }
}

/// Validate a single constraint in a where clause
fn validate_constraint(
    constraint: &Constraint,
    context_id: semantic_tree::symbol::SymbolId,
    db: &SemanticDatabase,
    file_id: usize,
    diagnostics: &mut DiagnosticContext,
) {
    match constraint {
        Constraint::TypeBound { param: _, bounds } => {
            // Validate each bound type
            for bound in bounds {
                validate_bound_type(bound, context_id, db, file_id, diagnostics);
            }
        }
    }
}

/// Validate that a bound type is a valid protocol
fn validate_bound_type(
    bound: &kestrel_semantic_tree::ty::Ty,
    context_id: semantic_tree::symbol::SymbolId,
    db: &SemanticDatabase,
    file_id: usize,
    diagnostics: &mut DiagnosticContext,
) {
    match bound.kind() {
        // Path types need to be resolved and checked
        TyKind::Path(segments, _type_args) => {
            // Resolve the type path
            let resolution = db.resolve_type_path(segments.clone(), context_id);

            match resolution {
                TypePathResolution::Resolved(ty) => {
                    // Check if the resolved type is a protocol
                    match ty.kind() {
                        TyKind::Protocol { .. } => {
                            // Valid protocol bound
                        }
                        TyKind::Struct { symbol, .. } => {
                            let error = NonProtocolBoundError {
                                type_name: symbol.metadata().name().value.clone(),
                                type_kind: "struct".to_string(),
                                span: bound.span().clone(),
                            };
                            diagnostics.throw(error, file_id);
                        }
                        TyKind::TypeAlias { symbol, .. } => {
                            let error = NonProtocolBoundError {
                                type_name: symbol.metadata().name().value.clone(),
                                type_kind: "type alias".to_string(),
                                span: bound.span().clone(),
                            };
                            diagnostics.throw(error, file_id);
                        }
                        TyKind::TypeParameter(param) => {
                            let error = NonProtocolBoundError {
                                type_name: param.metadata().name().value.clone(),
                                type_kind: "type parameter".to_string(),
                                span: bound.span().clone(),
                            };
                            diagnostics.throw(error, file_id);
                        }
                        _ => {
                            // Other types (primitives, tuples, functions) can't be bounds
                            let type_name = segments.join(".");
                            let error = NonProtocolBoundError {
                                type_name,
                                type_kind: "type".to_string(),
                                span: bound.span().clone(),
                            };
                            diagnostics.throw(error, file_id);
                        }
                    }
                }
                TypePathResolution::NotFound { segment, .. } => {
                    // Type not found - report unresolved bound error
                    let error = NonProtocolBoundError {
                        type_name: segment,
                        type_kind: "unresolved type".to_string(),
                        span: bound.span().clone(),
                    };
                    diagnostics.throw(error, file_id);
                }
                TypePathResolution::Ambiguous { segment, .. } => {
                    // Ambiguous type - report as error
                    let error = NonProtocolBoundError {
                        type_name: segment,
                        type_kind: "ambiguous type".to_string(),
                        span: bound.span().clone(),
                    };
                    diagnostics.throw(error, file_id);
                }
                TypePathResolution::NotAType { .. } => {
                    // Not a type - report error
                    let type_name = segments.join(".");
                    let error = NonProtocolBoundError {
                        type_name,
                        type_kind: "non-type symbol".to_string(),
                        span: bound.span().clone(),
                    };
                    diagnostics.throw(error, file_id);
                }
            }
        }
        // Protocol types are already resolved and valid
        TyKind::Protocol { .. } => {
            // Valid protocol bound
        }
        // All other type kinds are not valid as bounds
        _ => {
            let error = NonProtocolBoundError {
                type_name: format!("{:?}", bound.kind()),
                type_kind: "invalid type".to_string(),
                span: bound.span().clone(),
            };
            diagnostics.throw(error, file_id);
        }
    }
}

/// Get the file_id for a symbol by walking up to its SourceFile parent
fn get_file_id_for_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &DiagnosticContext,
) -> usize {
    let mut current = symbol.clone();
    loop {
        if current.metadata().kind() == KestrelSymbolKind::SourceFile {
            let file_name = current.metadata().name().value.clone();
            return diagnostics.get_file_id(&file_name).unwrap_or(0);
        }
        match current.metadata().parent() {
            Some(parent) => current = parent,
            None => return 0,
        }
    }
}
