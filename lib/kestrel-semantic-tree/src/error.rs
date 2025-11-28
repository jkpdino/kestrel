//! Error types for import resolution and semantic analysis

use kestrel_reporting::{Diagnostic, IntoDiagnostic, Label};
use kestrel_span::Span;

/// Error when an imported module path cannot be resolved
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleNotFoundError {
    /// The full import path that failed (e.g., ["A", "B", "C"])
    pub path: Vec<String>,
    /// The segment index where resolution failed (0-based)
    pub failed_segment_index: usize,
    /// Span of the entire module path in import statement
    pub path_span: Span,
    /// Span of the specific segment that failed
    pub failed_segment_span: Span,
}

impl IntoDiagnostic for ModuleNotFoundError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let failed_segment = &self.path[self.failed_segment_index];

        let partial_path = if self.failed_segment_index == 0 {
            failed_segment.clone()
        } else {
            self.path[..=self.failed_segment_index].join(".")
        };

        Diagnostic::error()
            .with_message(format!("module '{}' not found", partial_path))
            .with_labels(vec![
                Label::primary(file_id, self.failed_segment_span.clone())
                    .with_message(format!("no module named '{}'", failed_segment)),
                Label::secondary(file_id, self.path_span.clone())
                    .with_message("in this import"),
            ])
            .with_notes(vec![
                format!(
                    "the module '{}' does not exist or is not visible from this scope",
                    partial_path
                ),
            ])
    }
}

/// Error when trying to import a symbol that doesn't exist in the module
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolNotFoundInModuleError {
    /// The symbol name that wasn't found
    pub symbol_name: String,
    /// The module path where we looked
    pub module_path: Vec<String>,
    /// Span of the symbol name
    pub symbol_span: Span,
    /// Span of the module path
    pub module_span: Span,
}

impl IntoDiagnostic for SymbolNotFoundInModuleError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let module_name = self.module_path.join(".");

        Diagnostic::error()
            .with_message(format!(
                "symbol '{}' not found in module '{}'",
                self.symbol_name, module_name
            ))
            .with_labels(vec![
                Label::primary(file_id, self.symbol_span.clone())
                    .with_message(format!("'{}' does not exist", self.symbol_name)),
                Label::secondary(file_id, self.module_span.clone())
                    .with_message(format!("in module '{}'", module_name)),
            ])
    }
}

/// Error when trying to import from something that isn't a module
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CannotImportFromNonModuleError {
    /// What we tried to import from
    pub symbol_kind: String,
    /// Path to the symbol
    pub path: Vec<String>,
    /// Span of the path
    pub path_span: Span,
}

impl IntoDiagnostic for CannotImportFromNonModuleError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let path_str = self.path.join(".");

        Diagnostic::error()
            .with_message(format!(
                "cannot import from '{}': not a module",
                path_str
            ))
            .with_labels(vec![
                Label::primary(file_id, self.path_span.clone())
                    .with_message(format!("this is a {}, not a module", self.symbol_kind)),
            ])
            .with_notes(vec![
                "only modules can be imported from".to_string(),
            ])
    }
}

/// Error when an import creates a name conflict
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportConflictError {
    /// The conflicting name
    pub name: String,
    /// Span of the new import
    pub import_span: Span,
    /// Span of the existing declaration/import
    pub existing_span: Span,
    /// Whether the existing symbol is a declaration or import
    pub existing_is_import: bool,
}

impl IntoDiagnostic for ImportConflictError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let existing_kind = if self.existing_is_import {
            "imported"
        } else {
            "declared"
        };

        Diagnostic::error()
            .with_message(format!("'{}' is already {}", self.name, existing_kind))
            .with_labels(vec![
                Label::primary(file_id, self.import_span.clone())
                    .with_message(format!("cannot import '{}'", self.name)),
                Label::secondary(file_id, self.existing_span.clone())
                    .with_message(format!("'{}' first {} here", self.name, existing_kind)),
            ])
    }
}

/// Error when the imported symbol is not visible from the current scope
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolNotVisibleError {
    /// The symbol name
    pub symbol_name: String,
    /// The symbol's visibility level
    pub visibility: String,
    /// Span of the import statement
    pub import_span: Span,
    /// Span of the symbol's declaration (where visibility is declared)
    pub declaration_span: Option<Span>,
    /// File ID of the declaration (for cross-file diagnostics)
    pub declaration_file_id: Option<usize>,
}

impl IntoDiagnostic for SymbolNotVisibleError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let mut labels = vec![
            Label::primary(file_id, self.import_span.clone())
                .with_message(format!("'{}' is {}", self.symbol_name, self.visibility)),
        ];

        if let Some(decl_span) = &self.declaration_span {
            // Use declaration_file_id if available, otherwise fall back to import file
            let decl_file_id = self.declaration_file_id.unwrap_or(file_id);
            labels.push(
                Label::secondary(decl_file_id, decl_span.clone())
                    .with_message(format!("'{}' declared as {} here", self.symbol_name, self.visibility)),
            );
        }

        Diagnostic::error()
            .with_message(format!("'{}' is not accessible", self.symbol_name))
            .with_labels(labels)
    }
}

/// A participant in a circular type alias chain
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CycleParticipant {
    /// Name of the type alias
    pub name: String,
    /// Span of the type alias declaration's name
    pub name_span: Span,
    /// File ID where this type alias is declared
    pub file_id: Option<usize>,
}

/// Error when type aliases form a circular dependency
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CircularTypeAliasError {
    /// The type alias where the cycle was detected (the one being resolved)
    pub origin: CycleParticipant,
    /// The chain of type aliases that form the cycle, in order of resolution.
    /// Does not include the origin (which would be a duplicate at the end).
    pub cycle: Vec<CycleParticipant>,
}

impl IntoDiagnostic for CircularTypeAliasError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let cycle_names: Vec<_> = std::iter::once(&self.origin)
            .chain(self.cycle.iter())
            .map(|p| p.name.as_str())
            .collect();
        let cycle_display = cycle_names.join(" -> ");

        let mut labels = vec![Label::primary(file_id, self.origin.name_span.clone())
            .with_message("cycle starts here")];

        // Add secondary labels for each participant in the cycle
        for (i, participant) in self.cycle.iter().enumerate() {
            let participant_file_id = participant.file_id.unwrap_or(file_id);
            let message = if i == self.cycle.len() - 1 {
                format!("'{}' refers back to '{}'", participant.name, self.origin.name)
            } else {
                format!(
                    "'{}' refers to '{}'",
                    participant.name,
                    self.cycle.get(i + 1).map(|p| p.name.as_str()).unwrap_or(&self.origin.name)
                )
            };
            labels.push(
                Label::secondary(participant_file_id, participant.name_span.clone())
                    .with_message(message),
            );
        }

        Diagnostic::error()
            .with_message(format!(
                "circular type alias: {} -> {}",
                cycle_display, self.origin.name
            ))
            .with_labels(labels)
            .with_notes(vec![
                "type aliases cannot reference themselves, directly or indirectly".to_string(),
            ])
    }
}
