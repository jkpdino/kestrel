//! Member access errors.
//!
//! Errors related to accessing members (fields, methods, etc.) on types.

use kestrel_reporting::{Diagnostic, IntoDiagnostic, Label};
use kestrel_span::Span;

/// Error when a member cannot be found on a type.
pub struct NoSuchMemberError {
    /// Span of the member name being accessed
    pub member_span: Span,
    /// Name of the member being accessed
    pub member_name: String,
    /// Span of the base expression
    pub base_span: Span,
    /// String representation of the base type
    pub base_type: String,
}

impl IntoDiagnostic for NoSuchMemberError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "no member '{}' on type '{}'",
                self.member_name, self.base_type
            ))
            .with_labels(vec![
                Label::primary(file_id, self.member_span.clone())
                    .with_message("unknown member"),
                Label::secondary(file_id, self.base_span.clone())
                    .with_message(format!("has type '{}'", self.base_type)),
            ])
    }
}

/// Error when a member exists but is not visible from the current scope.
pub struct MemberNotVisibleError {
    /// Span of the member name being accessed
    pub member_span: Span,
    /// Name of the member being accessed
    pub member_name: String,
    /// Span of the base expression
    pub base_span: Span,
    /// String representation of the base type
    pub base_type: String,
    /// The visibility of the member
    pub visibility: String,
}

impl IntoDiagnostic for MemberNotVisibleError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "member '{}' is {} and not accessible from this scope",
                self.member_name, self.visibility
            ))
            .with_labels(vec![
                Label::primary(file_id, self.member_span.clone())
                    .with_message(format!("{} member", self.visibility)),
                Label::secondary(file_id, self.base_span.clone())
                    .with_message(format!("has type '{}'", self.base_type)),
            ])
    }
}

/// Error when a member exists but is not accessible as a value.
pub struct MemberNotAccessibleError {
    /// Span of the member name being accessed
    pub member_span: Span,
    /// Name of the member being accessed
    pub member_name: String,
    /// Span of the base expression
    pub base_span: Span,
    /// String representation of the base type
    pub base_type: String,
    /// What kind of member it is (e.g., "type alias", "associated type")
    pub member_kind: String,
}

impl IntoDiagnostic for MemberNotAccessibleError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "'{}' is a {} and cannot be used as a value",
                self.member_name, self.member_kind
            ))
            .with_labels(vec![
                Label::primary(file_id, self.member_span.clone())
                    .with_message(format!("is a {}", self.member_kind)),
                Label::secondary(file_id, self.base_span.clone())
                    .with_message(format!("has type '{}'", self.base_type)),
            ])
    }
}

/// Error when trying to access a member on a type that doesn't support member access.
pub struct CannotAccessMemberOnTypeError {
    /// Span of the entire access expression
    pub span: Span,
    /// String representation of the base type
    pub base_type: String,
}

impl IntoDiagnostic for CannotAccessMemberOnTypeError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "cannot access member on type '{}'",
                self.base_type
            ))
            .with_labels(vec![Label::primary(file_id, self.span.clone())
                .with_message("member access not supported")])
            .with_notes(vec![format!(
                "type '{}' does not have accessible members",
                self.base_type
            )])
    }
}
