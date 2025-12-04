//! Protocol-related errors.
//!
//! Errors related to protocol conformance, inheritance, and method implementation.

use kestrel_reporting::{Diagnostic, IntoDiagnostic, Label};
use kestrel_span::Span;

/// Error when a type is used where a protocol is expected.
pub struct NotAProtocolError {
    pub span: Span,
    pub name: String,
    /// Context where the protocol was expected (e.g., "bound", "conformance", "inheritance")
    pub context: NotAProtocolContext,
}

/// The context where a protocol was expected.
#[derive(Clone, Copy)]
pub enum NotAProtocolContext {
    /// Used as a generic type bound (e.g., `T: SomeStruct`)
    Bound,
    /// Used as a struct conformance (e.g., `struct Foo: SomeStruct`)
    Conformance,
    /// Used in protocol inheritance (e.g., `protocol Bar: SomeStruct`)
    Inheritance,
}

impl IntoDiagnostic for NotAProtocolError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let context_msg = match self.context {
            NotAProtocolContext::Bound => "cannot be used as a type bound",
            NotAProtocolContext::Conformance => "cannot be used as a conformance",
            NotAProtocolContext::Inheritance => "cannot be inherited by a protocol",
        };

        Diagnostic::error()
            .with_message(format!("'{}' is not a protocol", self.name))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message(context_msg)
            ])
    }
}

/// Error when a protocol has circular inheritance.
pub struct CircularProtocolInheritanceError {
    pub span: Span,
    pub protocol_name: String,
    /// The chain of protocols that form the cycle
    pub cycle: Vec<String>,
}

impl IntoDiagnostic for CircularProtocolInheritanceError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let cycle_str = self.cycle.join(" -> ");

        Diagnostic::error()
            .with_message(format!("protocol '{}' has circular inheritance", self.protocol_name))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message("circular inheritance detected")
            ])
            .with_notes(vec![
                format!("inheritance cycle: {}", cycle_str)
            ])
    }
}

/// Error when a struct doesn't implement a required protocol method.
pub struct MissingProtocolMethodError {
    pub span: Span,
    pub struct_name: String,
    pub protocol_name: String,
    pub method_name: String,
}

impl IntoDiagnostic for MissingProtocolMethodError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "type '{}' does not implement method '{}' from protocol '{}'",
                self.struct_name, self.method_name, self.protocol_name
            ))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message(format!("missing method '{}'", self.method_name))
            ])
    }
}

/// Error when an implemented method has the wrong return type.
pub struct WrongMethodReturnTypeError {
    pub span: Span,
    pub method_name: String,
    pub protocol_name: String,
    pub expected_type: String,
    pub actual_type: String,
}

impl IntoDiagnostic for WrongMethodReturnTypeError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "method '{}' has wrong return type for protocol '{}'",
                self.method_name, self.protocol_name
            ))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message(format!(
                        "expected '{}', found '{}'",
                        self.expected_type, self.actual_type
                    ))
            ])
    }
}

/// Error when a protocol method has a body.
pub struct ProtocolMethodHasBodyError {
    pub span: Span,
    pub method_name: String,
    pub protocol_name: String,
}

impl IntoDiagnostic for ProtocolMethodHasBodyError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "protocol method '{}' in '{}' cannot have a body",
                self.method_name, self.protocol_name
            ))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone())
                    .with_message("body not allowed in protocol method")
            ])
    }
}
