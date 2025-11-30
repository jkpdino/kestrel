//! Validation pass for function bodies
//!
//! Ensures that functions outside of protocols have bodies.

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::function_data::FunctionDataBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass that ensures functions have bodies (except in protocols)
pub struct FunctionBodyPass;

impl FunctionBodyPass {
    const NAME: &'static str = "function_body";
}

impl ValidationPass for FunctionBodyPass {
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
/// `in_protocol` tracks whether we're inside a protocol (where bodies are forbidden)
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
    in_protocol: bool,
) {
    let kind = symbol.metadata().kind();

    // Track if we're entering a protocol
    let in_protocol = in_protocol || kind == KestrelSymbolKind::Protocol;

    // Check functions
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
            // Only check functions outside protocols
            if !in_protocol && !data.has_body() {
                let name = &symbol.metadata().name().value;
                let span = symbol.metadata().declaration_span().clone();
                let file_id = get_file_id_for_symbol(symbol, diagnostics);

                let message = if config.debug_mode {
                    format!(
                        "[{}] function '{}' requires a body",
                        FunctionBodyPass::NAME,
                        name
                    )
                } else {
                    format!("function '{}' requires a body", name)
                };

                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)
                        .with_message("function declared without body")]);

                diagnostics.add_diagnostic(diagnostic);
            }
        }
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, diagnostics, config, in_protocol);
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
