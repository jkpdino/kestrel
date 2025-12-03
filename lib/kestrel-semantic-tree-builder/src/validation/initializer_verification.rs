//! Validation pass for initializer verification
//!
//! This pass verifies that initializers correctly initialize all fields:
//! - All fields must be assigned before the initializer returns
//! - `let` fields can only be assigned once
//! - Fields cannot be read before they are assigned
//! - Methods cannot be called on `self` before all fields are initialized
//!
//! TODO: This pass is a skeleton. Full implementation requires:
//! - Assignment expressions/statements to be implemented
//! - Control flow analysis for if/else branches
//! - Early return handling

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
    // TODO: Once control flow is added, this needs to be rewritten to handle:
    // - if/else branches (must init same fields in both branches)
    // - early returns (must have all fields initialized before return)
    // - loops
    for stmt in &body.statements {
        analyze_statement(stmt, &mut ctx);
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
    #[allow(dead_code)]
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
/// TODO: This needs to be expanded when assignment expressions are added.
fn analyze_expression(
    expr: &Expression,
    ctx: &mut VerificationContext,
    _is_assignment_target: bool,
) {
    match &expr.kind {
        ExprKind::FieldAccess { object, field } => {
            // Check if this is `self.field`
            if is_self_expr(object) {
                // This is a field access on self
                // TODO: When assignments are implemented, we need to distinguish:
                // - self.field = value  (assignment - marks field as initialized)
                // - ... = self.field    (read - field must be initialized)
                //
                // For now, we treat all field accesses as reads and check they're initialized
                if !ctx.assigned_fields.contains(field) {
                    ctx.errors.push(InitializerError::FieldReadBeforeAssigned {
                        span: expr.span.clone(),
                        field_name: field.clone(),
                    });
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
            // Local variable reference - check if it's `self`
            // TODO: Need to check if this local is `self` and handle appropriately
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
        ExprKind::Error => {}
    }
}

/// Check if an expression is a reference to `self`
fn is_self_expr(_expr: &Expression) -> bool {
    // TODO: This needs to check if the expression is a LocalRef to the `self` local
    // For now, we can't reliably detect this without more context
    false
}

/// Check if a field is mutable (var vs let)
fn is_field_mutable(_field: &Arc<dyn Symbol<KestrelLanguage>>) -> bool {
    // TODO: Check the FieldSymbol for mutability
    // For now, assume all fields are mutable
    true
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

#[cfg(test)]
mod tests {
    // TODO: Add tests once assignment expressions are implemented
}
