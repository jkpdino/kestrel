//! Validation pass for visibility consistency
//!
//! Ensures that public APIs don't expose less-visible types:
//! - Public functions can't have private/internal return types
//! - Public functions can't have private/internal parameter types
//! - Public type aliases can't alias private/internal types
//! - Public fields can't have private/internal types

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::callable::CallableBehavior;
use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::{Visibility, VisibilityBehavior};
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass that ensures visibility consistency
pub struct VisibilityConsistencyPass;

impl VisibilityConsistencyPass {
    const NAME: &'static str = "visibility_consistency";
}

impl ValidationPass for VisibilityConsistencyPass {
    fn name(&self) -> &'static str {
        Self::NAME
    }

    fn validate(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        _db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        config: &ValidationConfig,
    ) {
        validate_symbol(root, diagnostics, config);
    }
}

/// Visibility level for comparison (higher = more visible)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum VisibilityLevel {
    Private = 1,
    Fileprivate = 2,
    Internal = 3,
    Public = 4,
}

impl VisibilityLevel {
    fn from_visibility(vis: Option<&Visibility>) -> Self {
        match vis {
            Some(Visibility::Public) => VisibilityLevel::Public,
            Some(Visibility::Internal) => VisibilityLevel::Internal,
            Some(Visibility::Fileprivate) => VisibilityLevel::Fileprivate,
            Some(Visibility::Private) => VisibilityLevel::Private,
            None => VisibilityLevel::Internal, // Default is internal
        }
    }

    fn name(&self) -> &'static str {
        match self {
            VisibilityLevel::Public => "public",
            VisibilityLevel::Internal => "internal",
            VisibilityLevel::Fileprivate => "fileprivate",
            VisibilityLevel::Private => "private",
        }
    }
}

/// Get the visibility level of a symbol
fn get_symbol_visibility_level(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> VisibilityLevel {
    let behaviors = symbol.metadata().behaviors();
    let vis = behaviors
        .iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::Visibility))
        .and_then(|b| b.as_ref().downcast_ref::<VisibilityBehavior>())
        .and_then(|vb| vb.visibility());
    VisibilityLevel::from_visibility(vis)
}

/// Get visibility level from a concrete symbol type
fn get_visibility_level_from_symbol<S: Symbol<KestrelLanguage>>(symbol: &Arc<S>) -> VisibilityLevel {
    let behaviors = symbol.metadata().behaviors();
    let vis = behaviors
        .iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::Visibility))
        .and_then(|b| b.as_ref().downcast_ref::<VisibilityBehavior>())
        .and_then(|vb| vb.visibility());
    VisibilityLevel::from_visibility(vis)
}

/// Check if a type exposes a less-visible symbol, returns the offending type name and visibility
fn find_less_visible_type(ty: &Ty, required_level: VisibilityLevel) -> Option<(String, VisibilityLevel)> {
    match ty.kind() {
        TyKind::Class(class_symbol) => {
            let level = get_visibility_level_from_symbol(class_symbol);
            if level < required_level {
                Some((class_symbol.metadata().name().value.clone(), level))
            } else {
                None
            }
        }
        TyKind::Struct(struct_symbol) => {
            let level = get_visibility_level_from_symbol(struct_symbol);
            if level < required_level {
                Some((struct_symbol.metadata().name().value.clone(), level))
            } else {
                None
            }
        }
        TyKind::Protocol(protocol_symbol) => {
            let level = get_visibility_level_from_symbol(protocol_symbol);
            if level < required_level {
                Some((protocol_symbol.metadata().name().value.clone(), level))
            } else {
                None
            }
        }
        TyKind::TypeAlias(alias_symbol) => {
            let level = get_visibility_level_from_symbol(alias_symbol);
            if level < required_level {
                Some((alias_symbol.metadata().name().value.clone(), level))
            } else {
                None
            }
        }
        TyKind::Tuple(elements) => {
            for elem in elements {
                if let Some(result) = find_less_visible_type(elem, required_level) {
                    return Some(result);
                }
            }
            None
        }
        TyKind::Function { params, return_type } => {
            for param in params {
                if let Some(result) = find_less_visible_type(param, required_level) {
                    return Some(result);
                }
            }
            find_less_visible_type(return_type, required_level)
        }
        // Primitive types and unresolved paths don't have visibility issues
        TyKind::Unit
        | TyKind::Never
        | TyKind::Int(_)
        | TyKind::Float(_)
        | TyKind::Bool
        | TyKind::String
        | TyKind::Path(_) => None,
    }
}

/// Recursively validate symbols
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let kind = symbol.metadata().kind();
    let symbol_level = get_symbol_visibility_level(symbol);

    // Only check public symbols (they have the strictest requirements)
    if symbol_level == VisibilityLevel::Public {
        match kind {
            KestrelSymbolKind::Function => {
                check_function_visibility(symbol, diagnostics, config);
            }
            KestrelSymbolKind::TypeAlias => {
                check_type_alias_visibility(symbol, diagnostics, config);
            }
            KestrelSymbolKind::Field => {
                check_field_visibility(symbol, diagnostics, config);
            }
            _ => {}
        }
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, diagnostics, config);
    }
}

/// Check that a public function doesn't expose less-visible types
fn check_function_visibility(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let name = &symbol.metadata().name().value;
    let file_id = get_file_id_for_symbol(symbol, diagnostics);
    let span = symbol.metadata().declaration_span().clone();

    // Get CallableBehavior for parameter and return types
    let behaviors = symbol.metadata().behaviors();
    let callable = behaviors.iter().find_map(|b| {
        if matches!(b.kind(), KestrelBehaviorKind::Callable) {
            b.as_ref().downcast_ref::<CallableBehavior>()
        } else {
            None
        }
    });

    if let Some(callable) = callable {
        // Check return type
        if let Some((type_name, type_level)) =
            find_less_visible_type(callable.return_type(), VisibilityLevel::Public)
        {
            let message = if config.debug_mode {
                format!(
                    "[{}] public function '{}' exposes {} type '{}'",
                    VisibilityConsistencyPass::NAME,
                    name,
                    type_level.name(),
                    type_name
                )
            } else {
                format!(
                    "public function '{}' exposes {} type '{}'",
                    name,
                    type_level.name(),
                    type_name
                )
            };

            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(message)
                .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())
                    .with_message("return type is less visible than function")]);

            diagnostics.add_diagnostic(diagnostic);
        }

        // Check parameter types
        for param in callable.parameters() {
            if let Some((type_name, type_level)) =
                find_less_visible_type(&param.ty, VisibilityLevel::Public)
            {
                let message = if config.debug_mode {
                    format!(
                        "[{}] public function '{}' exposes {} type '{}' in parameter",
                        VisibilityConsistencyPass::NAME,
                        name,
                        type_level.name(),
                        type_name
                    )
                } else {
                    format!(
                        "public function '{}' exposes {} type '{}' in parameter",
                        name,
                        type_level.name(),
                        type_name
                    )
                };

                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![kestrel_reporting::Label::primary(
                        file_id,
                        span.clone(),
                    )
                    .with_message("parameter type is less visible than function")]);

                diagnostics.add_diagnostic(diagnostic);
            }
        }
    }
}

/// Check that a public type alias doesn't expose a less-visible type
fn check_type_alias_visibility(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let name = &symbol.metadata().name().value;
    let file_id = get_file_id_for_symbol(symbol, diagnostics);
    let span = symbol.metadata().declaration_span().clone();

    // Get TypedBehavior for the aliased type
    let behaviors = symbol.metadata().behaviors();
    let typed = behaviors.iter().find_map(|b| {
        if matches!(b.kind(), KestrelBehaviorKind::Typed) {
            b.as_ref().downcast_ref::<TypedBehavior>()
        } else {
            None
        }
    });

    if let Some(typed) = typed {
        if let Some((type_name, type_level)) =
            find_less_visible_type(typed.ty(), VisibilityLevel::Public)
        {
            let message = if config.debug_mode {
                format!(
                    "[{}] public type alias '{}' exposes {} type '{}'",
                    VisibilityConsistencyPass::NAME,
                    name,
                    type_level.name(),
                    type_name
                )
            } else {
                format!(
                    "public type alias '{}' exposes {} type '{}'",
                    name,
                    type_level.name(),
                    type_name
                )
            };

            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(message)
                .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)
                    .with_message("aliased type is less visible than alias")]);

            diagnostics.add_diagnostic(diagnostic);
        }
    }
}

/// Check that a public field doesn't expose a less-visible type
fn check_field_visibility(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let name = &symbol.metadata().name().value;
    let file_id = get_file_id_for_symbol(symbol, diagnostics);
    let span = symbol.metadata().declaration_span().clone();

    // Get TypedBehavior for the field type
    let behaviors = symbol.metadata().behaviors();
    let typed = behaviors.iter().find_map(|b| {
        if matches!(b.kind(), KestrelBehaviorKind::Typed) {
            b.as_ref().downcast_ref::<TypedBehavior>()
        } else {
            None
        }
    });

    if let Some(typed) = typed {
        if let Some((type_name, type_level)) =
            find_less_visible_type(typed.ty(), VisibilityLevel::Public)
        {
            let message = if config.debug_mode {
                format!(
                    "[{}] public field '{}' exposes {} type '{}'",
                    VisibilityConsistencyPass::NAME,
                    name,
                    type_level.name(),
                    type_name
                )
            } else {
                format!(
                    "public field '{}' exposes {} type '{}'",
                    name,
                    type_level.name(),
                    type_name
                )
            };

            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(message)
                .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)
                    .with_message("field type is less visible than field")]);

            diagnostics.add_diagnostic(diagnostic);
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
