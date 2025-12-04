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
use crate::diagnostics::ProtocolMethodHasBodyError;
use crate::utils::get_file_id_for_symbol;
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
                let protocol_name = protocol.metadata().name().value.clone();

                diagnostics.throw(ProtocolMethodHasBodyError {
                    span,
                    method_name: name.clone(),
                    protocol_name,
                }, file_id);
            }
        }
    }
}

