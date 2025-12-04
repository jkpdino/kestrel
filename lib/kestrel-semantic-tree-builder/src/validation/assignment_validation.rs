//! Validation pass for assignment expressions
//!
//! This pass verifies that assignment expressions have valid targets:
//! - The target must be a mutable variable (`var`, not `let`)
//! - The target must be an lvalue (local variable, field access, etc.)
//! - For field access, the receiver must be mutable
//!
//! Type checking (RHS compatible with LHS) is handled elsewhere.

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::executable::ExecutableBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::expr::{ExprKind, Expression};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::stmt::{Statement, StatementKind};
use kestrel_semantic_tree::symbol::function::FunctionSymbol;
use kestrel_semantic_tree::symbol::initializer::InitializerSymbol;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::local::LocalId;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::diagnostics::{CannotAssignToExpressionError, CannotAssignToImmutableError, CannotAssignToImmutableFieldError};
use crate::utils::get_file_id_for_symbol;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass for assignment target validity
pub struct AssignmentValidationPass;

impl AssignmentValidationPass {
    const NAME: &'static str = "assignment_validation";
}

impl ValidationPass for AssignmentValidationPass {
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

/// Context for assignment validation within a function/initializer
struct ValidationContext<'a> {
    /// The function symbol (to look up locals)
    function: Option<&'a FunctionSymbol>,
    /// The initializer symbol (to look up locals)
    initializer: Option<&'a InitializerSymbol>,
    /// The file ID for diagnostics
    file_id: usize,
}

impl<'a> ValidationContext<'a> {
    /// Get the name of a local by ID (for error messages)
    fn local_name(&self, id: LocalId) -> Option<String> {
        if let Some(func) = self.function {
            func.get_local(id).map(|l| l.name().to_string())
        } else if let Some(init) = self.initializer {
            init.get_local(id).map(|l| l.name().to_string())
        } else {
            None
        }
    }
}

/// Recursively validate symbols, looking for functions/initializers with bodies
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    let kind = symbol.metadata().kind();

    match kind {
        KestrelSymbolKind::Function => {
            if let Some(func) = symbol.as_ref().downcast_ref::<FunctionSymbol>() {
                validate_function_body(symbol, func, db, diagnostics);
            }
        }
        KestrelSymbolKind::Initializer => {
            if let Some(init) = symbol.as_ref().downcast_ref::<InitializerSymbol>() {
                validate_initializer_body(symbol, init, db, diagnostics);
            }
        }
        _ => {}
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, db, diagnostics);
    }
}

/// Validate assignment targets in a function body
fn validate_function_body(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    func: &FunctionSymbol,
    _db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    // Get the executable behavior (body)
    let behaviors = symbol.metadata().behaviors();
    let Some(exec) = behaviors
        .iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::Executable))
        .and_then(|b| b.as_ref().downcast_ref::<ExecutableBehavior>())
    else {
        return;
    };

    let file_id = get_file_id_for_symbol(symbol, diagnostics);
    let ctx = ValidationContext {
        function: Some(func),
        initializer: None,
        file_id,
    };

    // Check all statements
    for stmt in &exec.body().statements {
        validate_statement(stmt, &ctx, diagnostics);
    }

    // Check yield expression
    if let Some(yield_expr) = exec.body().yield_expr() {
        validate_expression(yield_expr, &ctx, diagnostics);
    }
}

/// Validate assignment targets in an initializer body
fn validate_initializer_body(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    init: &InitializerSymbol,
    _db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    // Get the executable behavior (body)
    let behaviors = symbol.metadata().behaviors();
    let Some(exec) = behaviors
        .iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::Executable))
        .and_then(|b| b.as_ref().downcast_ref::<ExecutableBehavior>())
    else {
        return;
    };

    let file_id = get_file_id_for_symbol(symbol, diagnostics);
    let ctx = ValidationContext {
        function: None,
        initializer: Some(init),
        file_id,
    };

    // Check all statements
    for stmt in &exec.body().statements {
        validate_statement(stmt, &ctx, diagnostics);
    }

    // Check yield expression
    if let Some(yield_expr) = exec.body().yield_expr() {
        validate_expression(yield_expr, &ctx, diagnostics);
    }
}

/// Validate a statement, looking for assignment expressions
fn validate_statement(stmt: &Statement, ctx: &ValidationContext, diagnostics: &mut DiagnosticContext) {
    match &stmt.kind {
        StatementKind::Binding { value, .. } => {
            if let Some(value) = value {
                validate_expression(value, ctx, diagnostics);
            }
        }
        StatementKind::Expr(expr) => {
            validate_expression(expr, ctx, diagnostics);
        }
    }
}

/// Validate an expression, checking assignment targets
fn validate_expression(expr: &Expression, ctx: &ValidationContext, diagnostics: &mut DiagnosticContext) {
    match &expr.kind {
        ExprKind::Assignment { target, value } => {
            // Validate the target is a valid lvalue
            validate_assignment_target(target, ctx, diagnostics);
            // Recursively check the value expression
            validate_expression(value, ctx, diagnostics);
        }
        // Recursively check nested expressions
        ExprKind::Array(elements) => {
            for elem in elements {
                validate_expression(elem, ctx, diagnostics);
            }
        }
        ExprKind::Tuple(elements) => {
            for elem in elements {
                validate_expression(elem, ctx, diagnostics);
            }
        }
        ExprKind::Grouping(inner) => {
            validate_expression(inner, ctx, diagnostics);
        }
        ExprKind::Call { callee, arguments } => {
            validate_expression(callee, ctx, diagnostics);
            for arg in arguments {
                validate_expression(&arg.value, ctx, diagnostics);
            }
        }
        ExprKind::PrimitiveMethodCall { receiver, arguments, .. } => {
            validate_expression(receiver, ctx, diagnostics);
            for arg in arguments {
                validate_expression(&arg.value, ctx, diagnostics);
            }
        }
        ExprKind::ImplicitStructInit { arguments, .. } => {
            for arg in arguments {
                validate_expression(&arg.value, ctx, diagnostics);
            }
        }
        ExprKind::FieldAccess { object, .. } => {
            validate_expression(object, ctx, diagnostics);
        }
        ExprKind::MethodRef { receiver, .. } => {
            validate_expression(receiver, ctx, diagnostics);
        }
        // Leaf expressions - no nested expressions to check
        ExprKind::Literal(_)
        | ExprKind::LocalRef(_)
        | ExprKind::SymbolRef(_)
        | ExprKind::OverloadedRef(_)
        | ExprKind::TypeRef(_)
        | ExprKind::Error => {}
    }
}

/// Validate that an expression is a valid assignment target (lvalue)
fn validate_assignment_target(
    target: &Expression,
    ctx: &ValidationContext,
    diagnostics: &mut DiagnosticContext,
) {
    match &target.kind {
        ExprKind::LocalRef(local_id) => {
            // Use the expression's mutable field directly
            if !target.is_mutable() {
                let name = ctx
                    .local_name(*local_id)
                    .unwrap_or_else(|| "<unknown>".to_string());
                diagnostics.throw(
                    CannotAssignToImmutableError {
                        span: target.span.clone(),
                        variable_name: name,
                    },
                    ctx.file_id,
                );
            }
        }
        ExprKind::FieldAccess { object, field } => {
            // Special case: `self.field = value` in initializers is always allowed
            // (that's how fields get initialized, even `let` fields)
            let is_self_in_init = ctx.initializer.is_some() && is_self_expr(object);

            if !is_self_in_init && !target.is_mutable() {
                // Field access is immutable - either the field is `let` or the receiver is immutable
                diagnostics.throw(
                    CannotAssignToImmutableFieldError {
                        span: target.span.clone(),
                        field_name: field.clone(),
                    },
                    ctx.file_id,
                );
            }

            // Recursively validate the object expression for any nested assignments
            validate_expression(object, ctx, diagnostics);
        }
        // Invalid assignment targets - not lvalues at all
        ExprKind::Literal(_)
        | ExprKind::Array(_)
        | ExprKind::Tuple(_)
        | ExprKind::Grouping(_)
        | ExprKind::Call { .. }
        | ExprKind::PrimitiveMethodCall { .. }
        | ExprKind::ImplicitStructInit { .. }
        | ExprKind::MethodRef { .. }
        | ExprKind::SymbolRef(_)
        | ExprKind::OverloadedRef(_)
        | ExprKind::TypeRef(_)
        | ExprKind::Assignment { .. }
        | ExprKind::Error => {
            diagnostics.throw(
                CannotAssignToExpressionError {
                    span: target.span.clone(),
                },
                ctx.file_id,
            );
        }
    }
}

/// Check if an expression is a reference to `self`
///
/// This is used to allow `self.field = value` in initializers, where we're
/// initializing fields before `self` is fully constructed.
pub fn is_self_expr(expr: &Expression) -> bool {
    match &expr.kind {
        ExprKind::LocalRef(local_id) => {
            // The self parameter is always local 0 in initializers and methods
            local_id.index() == 0
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_self_expr() {
        // Create a LocalRef to local 0 (self) - self is always mutable in initializers
        let self_expr = Expression::new(
            ExprKind::LocalRef(LocalId::new(0)),
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..4,
            true, // mutable
        );
        assert!(is_self_expr(&self_expr));

        // Create a LocalRef to local 1 (not self)
        let other_expr = Expression::new(
            ExprKind::LocalRef(LocalId::new(1)),
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..4,
            false,
        );
        assert!(!is_self_expr(&other_expr));

        // A literal is not self
        let literal = Expression::integer(42, 0..2);
        assert!(!is_self_expr(&literal));
    }

    #[test]
    fn test_is_self_expr_in_field_access() {
        // Create self.field expression
        let self_expr = Expression::new(
            ExprKind::LocalRef(LocalId::new(0)),
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..4,
            true, // self is mutable
        );
        let field_access = Expression::field_access(
            self_expr,
            "count".to_string(),
            true, // field is mutable (var)
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..10,
        );

        // The object inside the field access should be self
        if let ExprKind::FieldAccess { object, field: _ } = &field_access.kind {
            assert!(is_self_expr(object), "Expected object to be self");
        } else {
            panic!("Expected FieldAccess");
        }
        // Field access on mutable self with mutable field should be mutable
        assert!(field_access.is_mutable());
    }

    #[test]
    fn test_assignment_with_self_field() {
        // Create self.field = value expression
        let self_expr = Expression::new(
            ExprKind::LocalRef(LocalId::new(0)),
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..4,
            true, // self is mutable
        );
        let field_access = Expression::field_access(
            self_expr,
            "count".to_string(),
            true, // field is mutable (var)
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..10,
        );
        let value = Expression::integer(0, 11..12);
        let assignment = Expression::assignment(field_access, value, 0..12);

        // Check the structure
        if let ExprKind::Assignment { target, value: _ } = &assignment.kind {
            if let ExprKind::FieldAccess { object, field } = &target.kind {
                assert!(is_self_expr(object), "Expected object to be self");
                assert_eq!(field, "count");
            } else {
                panic!("Expected target to be FieldAccess");
            }
        } else {
            panic!("Expected Assignment");
        }
    }

    #[test]
    fn test_field_mutability_composition() {
        // Mutable parent + mutable field = mutable
        let mutable_parent = Expression::local_ref(
            LocalId::new(0),
            kestrel_semantic_tree::ty::Ty::error(0..1),
            true,
            0..4,
        );
        let access1 = Expression::field_access(
            mutable_parent,
            "x".to_string(),
            true, // mutable field
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..6,
        );
        assert!(access1.is_mutable());

        // Mutable parent + immutable field = immutable
        let mutable_parent2 = Expression::local_ref(
            LocalId::new(0),
            kestrel_semantic_tree::ty::Ty::error(0..1),
            true,
            0..4,
        );
        let access2 = Expression::field_access(
            mutable_parent2,
            "x".to_string(),
            false, // immutable field (let)
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..6,
        );
        assert!(!access2.is_mutable());

        // Immutable parent + mutable field = immutable
        let immutable_parent = Expression::local_ref(
            LocalId::new(0),
            kestrel_semantic_tree::ty::Ty::error(0..1),
            false,
            0..4,
        );
        let access3 = Expression::field_access(
            immutable_parent,
            "x".to_string(),
            true, // mutable field
            kestrel_semantic_tree::ty::Ty::error(0..1),
            0..6,
        );
        assert!(!access3.is_mutable());
    }
}
