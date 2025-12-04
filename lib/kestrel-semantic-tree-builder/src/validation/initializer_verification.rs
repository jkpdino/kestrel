//! Validation pass for initializer verification
//!
//! This pass verifies that initializers correctly initialize all fields:
//! - All fields must be assigned before the initializer returns
//! - `let` fields can only be assigned once
//! - Fields cannot be read before they are assigned
//! - Methods cannot be called on `self` before all fields are initialized
//!
//! Note: Control flow analysis (if/else branches, early returns, loops) is not yet
//! implemented. The current analysis is conservative and linear.

use std::collections::HashSet;
use std::sync::Arc;

use kestrel_reporting::{Diagnostic, DiagnosticContext, IntoDiagnostic, Label};
use kestrel_semantic_tree::behavior::executable::{CodeBlock, ExecutableBehavior};
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::expr::{ExprKind, Expression};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::stmt::{Statement, StatementKind};
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_span::Span;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::utils::get_file_id_for_symbol;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass for initializer field initialization
pub struct InitializerVerificationPass;

impl InitializerVerificationPass {
    const NAME: &'static str = "initializer_verification";
}

impl ValidationPass for InitializerVerificationPass {
    fn name(&self) -> &'static str {
        Self::NAME
    }

    fn validate(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        _config: &ValidationConfig,
    ) {
        validate_symbol(root, db, diagnostics);
    }
}

/// Recursively validate symbols, looking for initializers
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    let kind = symbol.metadata().kind();

    // Check initializers
    if kind == KestrelSymbolKind::Initializer {
        validate_initializer(symbol, db, diagnostics);
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, db, diagnostics);
    }
}

/// Validate a single initializer
fn validate_initializer(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    _db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    // Get the parent struct to know what fields need to be initialized
    let Some(parent) = symbol.metadata().parent() else {
        return;
    };

    if parent.metadata().kind() != KestrelSymbolKind::Struct {
        return;
    }

    // Collect all fields that need to be initialized
    let fields: Vec<FieldInfo> = parent
        .metadata()
        .children()
        .into_iter()
        .filter(|c| c.metadata().kind() == KestrelSymbolKind::Field)
        .map(|f| {
            let name = f.metadata().name().value.clone();
            let is_let = !is_field_mutable(&f);
            FieldInfo { name, is_let }
        })
        .collect();

    // Get the executable behavior (body)
    let Some(body) = get_executable_body(symbol) else {
        // No body - this is an error but handled elsewhere
        return;
    };

    // Create verification context
    let mut ctx = VerificationContext {
        fields: fields.iter().map(|f| f.name.clone()).collect(),
        let_fields: fields
            .iter()
            .filter(|f| f.is_let)
            .map(|f| f.name.clone())
            .collect(),
        assigned_fields: HashSet::new(),
        let_fields_assigned: HashSet::new(),
        errors: Vec::new(),
    };

    // Analyze the body
    for stmt in &body.statements {
        analyze_statement(stmt, &mut ctx);
    }

    // Also analyze the yield expression if present
    // (This handles cases where the last expression doesn't have a semicolon)
    if let Some(ref yield_expr) = body.yield_expr {
        analyze_expression(yield_expr, &mut ctx, false);
    }

    // Check that all fields are initialized
    let uninitialized: Vec<&String> = ctx
        .fields
        .iter()
        .filter(|f| !ctx.assigned_fields.contains(*f))
        .collect();

    if !uninitialized.is_empty() {
        let file_id = get_file_id_for_symbol(symbol, diagnostics);
        let span = symbol.metadata().span().clone();

        let field_list = uninitialized
            .iter()
            .map(|s| format!("'{}'", s))
            .collect::<Vec<_>>()
            .join(", ");

        let error = UninitializedFieldsError {
            span,
            fields: field_list,
        };
        diagnostics.add_diagnostic(error.into_diagnostic(file_id));
    }

    // Report any errors collected during analysis
    let file_id = get_file_id_for_symbol(symbol, diagnostics);
    for error in ctx.errors {
        diagnostics.add_diagnostic(error.into_diagnostic(file_id));
    }
}

/// Information about a field
struct FieldInfo {
    name: String,
    is_let: bool,
}

/// Context for tracking field initialization state
struct VerificationContext {
    /// All field names that need to be initialized
    fields: HashSet<String>,
    /// Fields declared with `let` (can only be assigned once)
    let_fields: HashSet<String>,
    /// Fields that have been assigned
    assigned_fields: HashSet<String>,
    /// `let` fields that have been assigned (to detect double assignment)
    let_fields_assigned: HashSet<String>,
    /// Collected errors
    errors: Vec<InitializerError>,
}

/// Errors that can occur during initializer verification
enum InitializerError {
    LetFieldAssignedTwice { span: Span, field_name: String },
    FieldReadBeforeAssigned { span: Span, field_name: String },
    SelfUsedBeforeFullyInitialized { span: Span, uninitialized: Vec<String> },
}

impl IntoDiagnostic for InitializerError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        match self {
            InitializerError::LetFieldAssignedTwice { span, field_name } => Diagnostic::error()
                .with_message(format!(
                    "cannot assign to 'let' field '{}' more than once",
                    field_name
                ))
                .with_labels(vec![
                    Label::primary(file_id, span.clone()).with_message("second assignment here")
                ]),
            InitializerError::FieldReadBeforeAssigned { span, field_name } => Diagnostic::error()
                .with_message(format!(
                    "cannot read field '{}' before it is initialized",
                    field_name
                ))
                .with_labels(vec![
                    Label::primary(file_id, span.clone()).with_message("field read here")
                ]),
            InitializerError::SelfUsedBeforeFullyInitialized { span, uninitialized } => {
                let fields = uninitialized.join(", ");
                Diagnostic::error()
                    .with_message("cannot use 'self' before all fields are initialized")
                    .with_labels(vec![
                        Label::primary(file_id, span.clone()).with_message("self used here")
                    ])
                    .with_notes(vec![format!("uninitialized fields: {}", fields)])
            }
        }
    }
}

/// Error for uninitialized fields at end of initializer
struct UninitializedFieldsError {
    span: Span,
    fields: String,
}

impl IntoDiagnostic for UninitializedFieldsError {
    fn into_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "initializer does not initialize all fields: {}",
                self.fields
            ))
            .with_labels(vec![
                Label::primary(file_id, self.span.clone()).with_message("in this initializer")
            ])
    }
}

/// Analyze a statement for field assignments and reads
fn analyze_statement(stmt: &Statement, ctx: &mut VerificationContext) {
    match &stmt.kind {
        StatementKind::Binding { pattern: _, value } => {
            // Variable binding - analyze the value expression
            if let Some(expr) = value {
                analyze_expression(expr, ctx, false);
            }
        }
        StatementKind::Expr(expr) => {
            // Expression statement - this is where assignments would be
            analyze_expression(expr, ctx, false);
        }
    }
}

/// Analyze an expression for field assignments and reads
///
/// `is_assignment_target` indicates if this expression is the LHS of an assignment.
fn analyze_expression(
    expr: &Expression,
    ctx: &mut VerificationContext,
    is_assignment_target: bool,
) {
    match &expr.kind {
        ExprKind::FieldAccess { object, field } => {
            // Check if this is `self.field`
            if is_self_expr(object) {
                if is_assignment_target {
                    // This is `self.field = value` - marks field as initialized
                    // Don't check if already initialized here; that's done in Assignment case
                } else {
                    // This is a read of self.field - must be initialized first
                    if !ctx.assigned_fields.contains(field) {
                        ctx.errors.push(InitializerError::FieldReadBeforeAssigned {
                            span: expr.span.clone(),
                            field_name: field.clone(),
                        });
                    }
                }
            } else {
                // Analyze the object expression
                analyze_expression(object, ctx, false);
            }
        }
        ExprKind::Call { callee, arguments } => {
            // Check if this is a method call on self
            if let ExprKind::MethodRef { receiver, .. } = &callee.kind {
                if is_self_expr(receiver) {
                    // Method call on self - all fields must be initialized
                    let uninitialized: Vec<String> = ctx
                        .fields
                        .iter()
                        .filter(|f| !ctx.assigned_fields.contains(*f))
                        .cloned()
                        .collect();

                    if !uninitialized.is_empty() {
                        ctx.errors
                            .push(InitializerError::SelfUsedBeforeFullyInitialized {
                                span: expr.span.clone(),
                                uninitialized,
                            });
                    }
                }
            }

            // Analyze callee and arguments
            analyze_expression(callee, ctx, false);
            for arg in arguments {
                analyze_expression(&arg.value, ctx, false);
            }
        }
        ExprKind::LocalRef(_) => {
            // Local variable reference - nothing to check here for field initialization
            // (self references in field access are handled in FieldAccess case)
        }
        ExprKind::SymbolRef(_) => {}
        ExprKind::TypeRef(_) => {}
        ExprKind::OverloadedRef(_) => {}
        ExprKind::MethodRef { receiver, .. } => {
            analyze_expression(receiver, ctx, false);
        }
        ExprKind::Literal(_) => {}
        ExprKind::Array(elements) => {
            for elem in elements {
                analyze_expression(elem, ctx, false);
            }
        }
        ExprKind::Tuple(elements) => {
            for elem in elements {
                analyze_expression(elem, ctx, false);
            }
        }
        ExprKind::Grouping(inner) => {
            analyze_expression(inner, ctx, false);
        }
        ExprKind::PrimitiveMethodCall {
            receiver,
            arguments,
            ..
        } => {
            analyze_expression(receiver, ctx, false);
            for arg in arguments {
                analyze_expression(&arg.value, ctx, false);
            }
        }
        ExprKind::ImplicitStructInit { arguments, .. } => {
            for arg in arguments {
                analyze_expression(&arg.value, ctx, false);
            }
        }
        ExprKind::Assignment { target, value } => {
            // Check if this is `self.field = value`
            if let ExprKind::FieldAccess { object, field } = &target.kind {
                if is_self_expr(object) {
                    // Check for double-assignment to `let` fields
                    if ctx.let_fields.contains(field) && ctx.let_fields_assigned.contains(field) {
                        ctx.errors.push(InitializerError::LetFieldAssignedTwice {
                            span: target.span.clone(),
                            field_name: field.clone(),
                        });
                    }
                    // Mark field as initialized
                    ctx.assigned_fields.insert(field.clone());
                    // Track let field assignments for double-assignment detection
                    if ctx.let_fields.contains(field) {
                        ctx.let_fields_assigned.insert(field.clone());
                    }
                }
            }
            // Analyze the target (but as an assignment target)
            analyze_expression(target, ctx, true);
            // Analyze the value being assigned
            analyze_expression(value, ctx, false);
        }
        ExprKind::Error => {}
    }
}

/// Check if an expression is a reference to `self`
///
/// In initializers and instance methods, `self` is always local 0.
fn is_self_expr(expr: &Expression) -> bool {
    use crate::validation::assignment_validation::is_self_expr as check_self;
    check_self(expr)
}

/// Check if a field is mutable (var vs let)
fn is_field_mutable(field: &Arc<dyn Symbol<KestrelLanguage>>) -> bool {
    use kestrel_semantic_tree::symbol::field::FieldSymbol;
    if let Some(field_sym) = field.as_ref().downcast_ref::<FieldSymbol>() {
        field_sym.is_mutable()
    } else {
        // If we can't downcast, assume mutable to avoid false positives
        true
    }
}

/// Get the executable body from a symbol
fn get_executable_body(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<CodeBlock> {
    let behaviors = symbol.metadata().behaviors();
    for b in behaviors.iter() {
        if matches!(b.kind(), KestrelBehaviorKind::Executable) {
            if let Some(exec) = b.as_ref().downcast_ref::<ExecutableBehavior>() {
                return Some(exec.body().clone());
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verification_context_tracks_fields() {
        let mut ctx = VerificationContext {
            fields: ["a", "b", "c"].iter().map(|s| s.to_string()).collect(),
            let_fields: ["a"].iter().map(|s| s.to_string()).collect(),
            assigned_fields: HashSet::new(),
            let_fields_assigned: HashSet::new(),
            errors: Vec::new(),
        };

        // Assign field 'a' (a let field)
        ctx.assigned_fields.insert("a".to_string());
        ctx.let_fields_assigned.insert("a".to_string());

        // Assign field 'b' (a var field)
        ctx.assigned_fields.insert("b".to_string());

        // Check uninitialized fields
        let uninitialized: Vec<&String> = ctx
            .fields
            .iter()
            .filter(|f| !ctx.assigned_fields.contains(*f))
            .collect();

        assert_eq!(uninitialized.len(), 1);
        assert!(uninitialized.contains(&&"c".to_string()));
    }

    #[test]
    fn test_let_field_double_assignment_detection() {
        let mut ctx = VerificationContext {
            fields: ["id"].iter().map(|s| s.to_string()).collect(),
            let_fields: ["id"].iter().map(|s| s.to_string()).collect(),
            assigned_fields: HashSet::new(),
            let_fields_assigned: HashSet::new(),
            errors: Vec::new(),
        };

        // First assignment to let field
        let field = "id".to_string();
        assert!(!ctx.let_fields_assigned.contains(&field));
        ctx.assigned_fields.insert(field.clone());
        ctx.let_fields_assigned.insert(field.clone());

        // Second assignment should be detected
        assert!(ctx.let_fields.contains(&field) && ctx.let_fields_assigned.contains(&field));
    }
}
