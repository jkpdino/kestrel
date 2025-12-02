//! Validation pass for static modifier context
//!
//! Ensures that the `static` keyword is only used inside structs or protocols.

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::function_data::FunctionDataBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::diagnostics::{StaticContext, StaticInWrongContextError};
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass that ensures static modifier is only used in valid contexts
pub struct StaticContextPass;

impl StaticContextPass {
    const NAME: &'static str = "static_context";
}

impl ValidationPass for StaticContextPass {
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
        validate_symbol(root, diagnostics, config, false);
    }
}

/// Recursively validate symbols
///
/// `in_valid_context` tracks whether we're inside a struct or protocol
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
    in_valid_context: bool,
) {
    let kind = symbol.metadata().kind();
    let name = &symbol.metadata().name().value;

    // Check if we're entering a valid context for static
    // Skip the root symbol (which is a placeholder Module with name "<root>")
    let is_valid_context = matches!(
        kind,
        KestrelSymbolKind::Struct | KestrelSymbolKind::Protocol
    ) && name != "<root>";
    let new_context = in_valid_context || is_valid_context;

    // Check functions for invalid static usage
    if kind == KestrelSymbolKind::Function {
        // Get the FunctionDataBehavior
        let behaviors = symbol.metadata().behaviors();
        let function_data = behaviors.iter().find_map(|b| {
            if matches!(b.kind(), KestrelBehaviorKind::FunctionData) {
                b.as_ref().downcast_ref::<FunctionDataBehavior>()
            } else {
                None
            }
        });

        if let Some(data) = function_data {
            if data.is_static() && !in_valid_context {
                let name = &symbol.metadata().name().value;
                let span = symbol.metadata().declaration_span().clone();
                let file_id = get_file_id_for_symbol(symbol, diagnostics);

                diagnostics.throw(StaticInWrongContextError {
                    span,
                    name: name.clone(),
                    context: StaticContext::ModuleLevel,
                }, file_id);
            }
        }
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, diagnostics, config, new_context);
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
