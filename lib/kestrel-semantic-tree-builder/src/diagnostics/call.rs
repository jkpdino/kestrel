//! Call-related errors.
//!
//! Errors related to function calls, method calls, and overload resolution.

use kestrel_reporting::{Diagnostic, IntoDiagnostic, Label};
use kestrel_span::Span;

/// A description of an available overload for error messages.
#[derive(Debug, Clone)]
pub struct OverloadDescription {
    /// The function/method name
    pub name: String,
    /// The parameter labels (None = unlabeled positional parameter)
    pub labels: Vec<Option<String>>,
    /// The parameter type descriptions
    pub param_types: Vec<String>,
    /// The span where this overload is defined (for secondary labels)
    pub definition_span: Option<Span>,
    /// The file ID where this overload is defined
    pub definition_file_id: Option<usize>,
}

impl OverloadDescription {
    /// Format the overload signature for display.
    pub fn display(&self) -> String {
        let params: Vec<String> = self
            .labels
            .iter()
            .zip(self.param_types.iter())
            .map(|(label, ty)| match label {
                Some(l) => format!("{}: {}", l, ty),
                None => ty.clone(),
            })
            .collect();

        format!("{}({})", self.name, params.join(", "))
    }
}

/// Format argument labels for display in error messages.
pub fn format_argument_labels(labels: &[Option<String>]) -> String {
    if labels.is_empty() {
        return "()".to_string();
    }

    let formatted: Vec<String> = labels
        .iter()
        .map(|l| match l {
            Some(label) => format!("{}:", label),
            None => "_".to_string(),
        })
        .collect();

    format!("({})", formatted.join(", "))
}

/// Error when no overload matches the provided arguments.
pub struct NoMatchingOverloadError {
    /// Span of the entire call expression
    pub call_span: Span,
    /// The function/method name being called
    pub name: String,
    /// The argument labels provided (None = unlabeled)
    pub provided_labels: Vec<Option<String>>,
    /// Number of arguments provided
    pub provided_arity: usize,
    /// Available overloads that didn't match
    pub available_overloads: Vec<OverloadDescription>,
}

impl IntoDiagnostic for NoMatchingOverloadError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let provided = format_argument_labels(&self.provided_labels);

        let mut labels = vec![Label::primary(file_id, self.call_span.clone()).with_message(
            format!(
                "no matching overload for {} arguments with labels {}",
                self.provided_arity, provided
            ),
        )];

        // Add secondary labels for available overloads (if we have location info)
        for overload in &self.available_overloads {
            if let (Some(span), Some(def_file_id)) =
                (&overload.definition_span, overload.definition_file_id)
            {
                labels.push(
                    Label::secondary(def_file_id, span.clone())
                        .with_message(format!("candidate: {}", overload.display())),
                );
            }
        }

        let mut notes = vec![];
        if !self.available_overloads.is_empty() {
            notes.push("available overloads:".to_string());
            for overload in &self.available_overloads {
                notes.push(format!("  - {}", overload.display()));
            }
        }

        Diagnostic::error()
            .with_message(format!(
                "no matching overload for '{}' with {} argument(s)",
                self.name, self.provided_arity
            ))
            .with_labels(labels)
            .with_notes(notes)
    }
}

/// Error when no method matches the provided arguments.
pub struct NoMatchingMethodError {
    /// Span of the entire call expression
    pub call_span: Span,
    /// The method name being called
    pub method_name: String,
    /// The receiver type
    pub receiver_type: String,
    /// The argument labels provided (None = unlabeled)
    pub provided_labels: Vec<Option<String>>,
    /// Number of arguments provided
    pub provided_arity: usize,
    /// Available method overloads that didn't match
    pub available_overloads: Vec<OverloadDescription>,
}

impl IntoDiagnostic for NoMatchingMethodError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let provided = format_argument_labels(&self.provided_labels);

        let mut labels = vec![Label::primary(file_id, self.call_span.clone()).with_message(
            format!(
                "no method '{}' with {} argument(s) and labels {}",
                self.method_name, self.provided_arity, provided
            ),
        )];

        // Add secondary labels for available overloads (if we have location info)
        for overload in &self.available_overloads {
            if let (Some(span), Some(def_file_id)) =
                (&overload.definition_span, overload.definition_file_id)
            {
                labels.push(
                    Label::secondary(def_file_id, span.clone())
                        .with_message(format!("candidate: {}", overload.display())),
                );
            }
        }

        let mut notes = vec![];
        if !self.available_overloads.is_empty() {
            notes.push(format!(
                "available methods on '{}':",
                self.receiver_type
            ));
            for overload in &self.available_overloads {
                notes.push(format!("  - {}", overload.display()));
            }
        }

        Diagnostic::error()
            .with_message(format!(
                "no method '{}' on type '{}' matches the provided arguments",
                self.method_name, self.receiver_type
            ))
            .with_labels(labels)
            .with_notes(notes)
    }
}

/// Error when a primitive method is called with wrong number of arguments.
pub struct PrimitiveMethodArityError {
    /// Span of the call expression
    pub call_span: Span,
    /// The method name
    pub method_name: String,
    /// The receiver type
    pub receiver_type: String,
    /// Expected number of arguments
    pub expected_arity: usize,
    /// Provided number of arguments
    pub provided_arity: usize,
}

impl IntoDiagnostic for PrimitiveMethodArityError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "method '{}' on '{}' takes {} argument(s), but {} were provided",
                self.method_name, self.receiver_type, self.expected_arity, self.provided_arity
            ))
            .with_labels(vec![Label::primary(file_id, self.call_span.clone())
                .with_message(format!("expected {} argument(s)", self.expected_arity))])
    }
}

/// Error when trying to access a primitive method without calling it.
pub struct PrimitiveMethodNotCallableError {
    /// Span of the member access expression
    pub span: Span,
    /// The method name
    pub method_name: String,
    /// The receiver type
    pub receiver_type: String,
}

impl IntoDiagnostic for PrimitiveMethodNotCallableError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "primitive method '{}' on '{}' must be called",
                self.method_name, self.receiver_type
            ))
            .with_labels(vec![Label::primary(file_id, self.span.clone())
                .with_message("add () to call this method")])
            .with_notes(vec![format!(
                "primitive methods cannot be used as first-class values; use {}.{}() instead",
                self.receiver_type, self.method_name
            )])
    }
}

/// Error when no method exists on a type.
pub struct NoSuchMethodError {
    /// Span of the method call
    pub call_span: Span,
    /// The method name being called
    pub method_name: String,
    /// The receiver type
    pub receiver_type: String,
}

impl IntoDiagnostic for NoSuchMethodError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "no method '{}' on type '{}'",
                self.method_name, self.receiver_type
            ))
            .with_labels(vec![Label::primary(file_id, self.call_span.clone())
                .with_message("method not found")])
    }
}
