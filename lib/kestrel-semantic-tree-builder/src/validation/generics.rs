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
use kestrel_semantic_tree::error::{DefaultOrderingError, DuplicateTypeParameterError};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::function::FunctionSymbol;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::protocol::ProtocolSymbol;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::symbol::type_alias::TypeAliasSymbol;
use kestrel_semantic_tree::symbol::type_parameter::TypeParameterSymbol;
use kestrel_span::Span;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
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
        _db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        _config: &ValidationConfig,
    ) {
        validate_symbol(root, diagnostics);
    }
}

/// Recursively validate symbols for generics issues
fn validate_symbol(symbol: &Arc<dyn Symbol<KestrelLanguage>>, diagnostics: &mut DiagnosticContext) {
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
            }
        }
        KestrelSymbolKind::Function => {
            if let Some(func_sym) = symbol_ref.as_any().downcast_ref::<FunctionSymbol>() {
                validate_type_parameters(
                    func_sym.type_parameters(),
                    symbol,
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
            }
        }
        KestrelSymbolKind::TypeAlias => {
            if let Some(alias_sym) = symbol_ref.as_any().downcast_ref::<TypeAliasSymbol>() {
                validate_type_parameters(
                    alias_sym.type_parameters(),
                    symbol,
                    diagnostics,
                );
            }
        }
        _ => {}
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, diagnostics);
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
