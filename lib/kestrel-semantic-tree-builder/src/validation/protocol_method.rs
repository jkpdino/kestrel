//! Validation pass for protocol methods
//!
//! Ensures that methods declared inside protocols do NOT have bodies.

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::function_data::FunctionDataBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass that ensures protocol methods don't have bodies
pub struct ProtocolMethodPass;

impl ProtocolMethodPass {
    const NAME: &'static str = "protocol_method";
}

impl ValidationPass for ProtocolMethodPass {
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

/// Recursively validate symbols, looking for protocols
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let kind = symbol.metadata().kind();

    // When we find a protocol, check its method children
    if kind == KestrelSymbolKind::Protocol {
        check_protocol_methods(symbol, diagnostics, config);
    }

    // Recursively check children (protocols can't be nested, but we still walk the tree)
    for child in symbol.metadata().children() {
        validate_symbol(&child, diagnostics, config);
    }
}

/// Check all function children of a protocol for forbidden bodies
fn check_protocol_methods(
    protocol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    for child in protocol.metadata().children() {
        if child.metadata().kind() != KestrelSymbolKind::Function {
            continue;
        }

        // Get the FunctionDataBehavior
        let behaviors = child.metadata().behaviors();
        let function_data = behaviors.iter().find_map(|b| {
            if matches!(b.kind(), KestrelBehaviorKind::FunctionData) {
                b.as_ref().downcast_ref::<FunctionDataBehavior>()
            } else {
                None
            }
        });

        if let Some(data) = function_data {
            if data.has_body() {
                let name = &child.metadata().name().value;
                let span = child.metadata().declaration_span().clone();
                let file_id = get_file_id_for_symbol(&child, diagnostics);

                let message = if config.debug_mode {
                    format!(
                        "[{}] protocol method '{}' cannot have a body",
                        ProtocolMethodPass::NAME,
                        name
                    )
                } else {
                    format!("protocol method '{}' cannot have a body", name)
                };

                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)
                        .with_message("body not allowed in protocol method")]);

                diagnostics.add_diagnostic(diagnostic);
            }
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
