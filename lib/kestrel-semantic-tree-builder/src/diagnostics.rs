//! Diagnostic types for semantic tree building errors

use kestrel_reporting::{Diagnostic, IntoDiagnostic, Label};
use kestrel_span::Span;

/// Diagnostic for when a file has no module declaration
pub struct NoModuleDeclarationError {
    pub span: Span,
}

impl IntoDiagnostic for NoModuleDeclarationError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("no module declaration found in file")
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message("module declaration should appear here")
            ])
            .with_notes(vec![
                "Every Kestrel file must start with a module declaration.".to_string(),
                "Example: module MyModule".to_string(),
            ])
    }
}

/// Diagnostic for when module declaration is not the first statement
pub struct ModuleNotFirstError {
    pub module_span: Span,
    pub first_item_span: Span,
    pub first_item_kind: String,
}

impl IntoDiagnostic for ModuleNotFirstError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("module declaration must be the first statement in the file")
            .with_labels(vec![
                Label::secondary(file_id, self.first_item_span.clone())
                    .with_message(format!("{} appears before module declaration", self.first_item_kind)),
                Label::primary(file_id, self.module_span.clone())
                    .with_message("module declaration should be first"),
            ])
            .with_notes(vec![
                "The module declaration must come before any imports or declarations.".to_string(),
            ])
    }
}

/// Diagnostic for when a file has multiple module declarations
pub struct MultipleModuleDeclarationsError {
    pub first_span: Span,
    pub duplicate_spans: Vec<Span>,
    pub count: usize,
}

impl IntoDiagnostic for MultipleModuleDeclarationsError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let mut labels = vec![
            Label::primary(file_id, self.first_span.clone())
                .with_message("first module declaration here"),
        ];

        for (i, span) in self.duplicate_spans.iter().enumerate() {
            labels.push(
                Label::secondary(file_id, span.clone())
                    .with_message(format!("duplicate module declaration #{}", i + 2))
            );
        }

        Diagnostic::error()
            .with_message(format!(
                "multiple module declarations found ({} total)",
                self.count
            ))
            .with_labels(labels)
            .with_notes(vec![
                "Only one module declaration is allowed per file.".to_string(),
            ])
    }
}

/// Diagnostic for when too many type arguments are provided
pub struct TooManyTypeArgumentsError {
    pub span: Span,
    pub type_name: String,
    pub expected: usize,
    pub got: usize,
}

impl IntoDiagnostic for TooManyTypeArgumentsError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "too many type arguments for '{}'",
                self.type_name
            ))
            .with_labels(vec![Label::primary(file_id, self.span.clone()).with_message(
                format!(
                    "expected {} type argument{}, found {}",
                    self.expected,
                    if self.expected == 1 { "" } else { "s" },
                    self.got
                ),
            )])
    }
}

/// Diagnostic for when too few type arguments are provided
pub struct TooFewTypeArgumentsError {
    pub span: Span,
    pub type_name: String,
    pub expected: usize,
    pub got: usize,
    pub first_missing: String,
}

impl IntoDiagnostic for TooFewTypeArgumentsError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "missing type argument{} for '{}'",
                if self.expected - self.got > 1 { "s" } else { "" },
                self.type_name
            ))
            .with_labels(vec![Label::primary(file_id, self.span.clone()).with_message(
                format!(
                    "expected {} type argument{}, found {}",
                    self.expected,
                    if self.expected == 1 { "" } else { "s" },
                    self.got
                ),
            )])
            .with_notes(vec![format!(
                "missing type argument for parameter '{}'",
                self.first_missing
            )])
    }
}

/// Diagnostic for when type arguments are provided to a non-generic type
pub struct NotGenericError {
    pub span: Span,
    pub type_name: String,
}

impl IntoDiagnostic for NotGenericError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "type '{}' does not accept type arguments",
                self.type_name
            ))
            .with_labels(vec![Label::primary(file_id, self.span.clone())
                .with_message("not a generic type")])
            .with_notes(vec![format!(
                "'{}' is not declared with type parameters",
                self.type_name
            )])
    }
}

/// Diagnostic for when a type cannot be found
pub struct UnresolvedTypeError {
    pub span: Span,
    pub type_name: String,
}

impl IntoDiagnostic for UnresolvedTypeError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot find type '{}' in this scope", self.type_name))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message("not found")
            ])
    }
}

/// Diagnostic for when a type name is ambiguous
pub struct AmbiguousTypeError {
    pub span: Span,
    pub type_name: String,
    pub candidate_count: usize,
}

impl IntoDiagnostic for AmbiguousTypeError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("type '{}' is ambiguous", self.type_name))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message(format!("{} types with this name in scope", self.candidate_count))
            ])
            .with_notes(vec![
                "Use a fully qualified path to disambiguate.".to_string()
            ])
    }
}

/// Diagnostic for when a symbol is not a type
pub struct NotATypeError {
    pub span: Span,
    pub name: String,
    pub actual_kind: String,
}

impl IntoDiagnostic for NotATypeError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("'{}' is not a type", self.name))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message(format!("'{}' is a {}, not a type", self.name, self.actual_kind))
            ])
    }
}
