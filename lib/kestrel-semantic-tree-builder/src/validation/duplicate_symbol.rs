//! Validation pass for duplicate symbols
//!
//! Ensures no duplicate symbols exist within a scope:
//! - No duplicate type names (struct, protocol, type alias)
//! - No duplicate member names (field, function) within a type
//!
//! Note: Function overloading (same name, different signature) is allowed
//! and handled separately by the existing `check_duplicate_signatures`.

use std::collections::HashMap;
use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass that ensures no duplicate symbols exist
pub struct DuplicateSymbolPass;

impl DuplicateSymbolPass {
    const NAME: &'static str = "duplicate_symbol";
}

impl ValidationPass for DuplicateSymbolPass {
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

/// Recursively validate symbols for duplicates
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let kind = symbol.metadata().kind();

    // Check for duplicate types in scopes that can contain types
    // (Module, SourceFile)
    if matches!(
        kind,
        KestrelSymbolKind::Module | KestrelSymbolKind::SourceFile
    ) {
        check_duplicate_types(symbol, diagnostics, config);
    }

    // Check for duplicate members in types
    if matches!(
        kind,
        KestrelSymbolKind::Struct | KestrelSymbolKind::Protocol
    ) {
        check_duplicate_members(symbol, diagnostics, config);
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, diagnostics, config);
    }
}

/// Check for duplicate type names within a scope
fn check_duplicate_types(
    scope: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    // Map from name to (first symbol, kind description)
    let mut types: HashMap<String, (Arc<dyn Symbol<KestrelLanguage>>, &'static str)> =
        HashMap::new();

    for child in scope.metadata().children() {
        let child_kind = child.metadata().kind();

        // Only check type-like symbols
        let kind_desc = match child_kind {
            KestrelSymbolKind::Struct => "struct",
            KestrelSymbolKind::Protocol => "protocol",
            KestrelSymbolKind::TypeAlias => "type alias",
            _ => continue,
        };

        let name = child.metadata().name().value.clone();

        if let Some((first, first_kind)) = types.get(&name) {
            // Duplicate found
            let file_id = get_file_id_for_symbol(&child, diagnostics);
            let first_file_id = get_file_id_for_symbol(first, diagnostics);

            let message = if config.debug_mode {
                format!(
                    "[{}] duplicate type '{}': already defined as {}",
                    DuplicateSymbolPass::NAME,
                    name,
                    first_kind
                )
            } else {
                format!("duplicate type '{}': already defined as {}", name, first_kind)
            };

            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(message)
                .with_labels(vec![
                    kestrel_reporting::Label::primary(
                        file_id,
                        child.metadata().declaration_span().clone(),
                    )
                    .with_message(format!("{} defined here", kind_desc)),
                    kestrel_reporting::Label::secondary(
                        first_file_id,
                        first.metadata().declaration_span().clone(),
                    )
                    .with_message(format!("first defined as {} here", first_kind)),
                ]);

            diagnostics.add_diagnostic(diagnostic);
        } else {
            types.insert(name, (child.clone(), kind_desc));
        }
    }
}

/// Check for duplicate member names within a type (struct, protocol)
fn check_duplicate_members(
    type_symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let type_name = &type_symbol.metadata().name().value;
    let type_kind = match type_symbol.metadata().kind() {
        KestrelSymbolKind::Struct => "struct",
        KestrelSymbolKind::Protocol => "protocol",
        _ => return,
    };

    // Map from name to (first symbol, kind description)
    // For functions, we only store the first one - signature duplicates are handled elsewhere
    let mut members: HashMap<String, (Arc<dyn Symbol<KestrelLanguage>>, &'static str)> =
        HashMap::new();

    for child in type_symbol.metadata().children() {
        let child_kind = child.metadata().kind();

        let kind_desc = match child_kind {
            KestrelSymbolKind::Field => "field",
            KestrelSymbolKind::Function => "function",
            _ => continue,
        };

        let name = child.metadata().name().value.clone();

        if let Some((first, first_kind)) = members.get(&name) {
            // For function-to-function duplicates, skip - handled by signature check
            if child_kind == KestrelSymbolKind::Function
                && first.metadata().kind() == KestrelSymbolKind::Function
            {
                continue;
            }

            // Duplicate found (field-field, field-function, or function-field)
            let file_id = get_file_id_for_symbol(&child, diagnostics);
            let first_file_id = get_file_id_for_symbol(first, diagnostics);

            let message = if config.debug_mode {
                format!(
                    "[{}] duplicate member '{}' in {} '{}': already defined as {}",
                    DuplicateSymbolPass::NAME,
                    name,
                    type_kind,
                    type_name,
                    first_kind
                )
            } else {
                format!(
                    "duplicate member '{}' in {} '{}': already defined as {}",
                    name, type_kind, type_name, first_kind
                )
            };

            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(message)
                .with_labels(vec![
                    kestrel_reporting::Label::primary(
                        file_id,
                        child.metadata().declaration_span().clone(),
                    )
                    .with_message(format!("{} defined here", kind_desc)),
                    kestrel_reporting::Label::secondary(
                        first_file_id,
                        first.metadata().declaration_span().clone(),
                    )
                    .with_message(format!("first defined as {} here", first_kind)),
                ]);

            diagnostics.add_diagnostic(diagnostic);
        } else {
            members.insert(name, (child.clone(), kind_desc));
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
