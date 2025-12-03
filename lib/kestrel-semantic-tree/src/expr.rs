//! Expression data types for the semantic tree.
//!
//! Expressions are plain data structures (not symbols) that represent
//! resolved expressions in function bodies. They are created during
//! the bind phase after path resolution.

use kestrel_span::Span;
use semantic_tree::symbol::SymbolId;

use crate::symbol::local::LocalId;
use crate::ty::Ty;

/// Represents a literal value in an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    /// Unit literal: `()`
    Unit,
    /// Integer literal: `42`, `0xFF`, `0b1010`, `0o17`
    Integer(i64),
    /// Float literal: `3.14`, `1.0e10`
    Float(f64),
    /// String literal: `"hello"`
    String(String),
    /// Boolean literal: `true`, `false`
    Bool(bool),
}

/// Represents the kind of expression.
///
/// All variants represent resolved expressions - there is no `Path` variant
/// because expressions are only created after path resolution during bind phase.
#[derive(Debug, Clone)]
pub enum ExprKind {
    // Literals
    /// Literal expression (integer, float, string, bool, unit)
    Literal(LiteralValue),
    /// Array literal: `[1, 2, 3]`
    Array(Vec<Expression>),
    /// Tuple literal: `(1, 2, 3)`
    Tuple(Vec<Expression>),
    /// Grouping expression: `(expr)`
    Grouping(Box<Expression>),

    // Resolved references
    /// Reference to a local variable (resolved from a path).
    /// The LocalId references the function's locals vector.
    LocalRef(LocalId),
    /// Reference to a symbol with ValueBehavior (resolved from a path).
    /// Used for module-level functions, fields, globals, etc.
    SymbolRef(SymbolId),
    /// Overloaded function reference (pending call resolution).
    /// Stores candidates that will be disambiguated by call arguments.
    OverloadedRef(Vec<SymbolId>),

    // Member access
    /// Field access: `obj.field`
    /// The object expression and the field name.
    FieldAccess {
        object: Box<Expression>,
        field: String,
    },

    // Future: If, While, Block, Call, BinaryOp, UnaryOp, etc.

    /// Error expression (poison value).
    /// Used when expression resolution fails - prevents cascading errors.
    Error,
}

/// A resolved expression in the semantic tree.
///
/// Unlike symbols, expressions are plain data structures without SymbolId.
/// They are created during the bind phase after path resolution.
#[derive(Debug, Clone)]
pub struct Expression {
    /// The kind of expression
    pub kind: ExprKind,
    /// The resolved type of this expression
    pub ty: Ty,
    /// The source span of this expression
    pub span: Span,
}

impl Expression {
    /// Create a new expression.
    pub fn new(kind: ExprKind, ty: Ty, span: Span) -> Self {
        Expression { kind, ty, span }
    }

    /// Create a unit literal expression.
    pub fn unit(span: Span) -> Self {
        Expression {
            kind: ExprKind::Literal(LiteralValue::Unit),
            ty: Ty::unit(span.clone()),
            span,
        }
    }

    /// Create an integer literal expression.
    pub fn integer(value: i64, span: Span) -> Self {
        Expression {
            kind: ExprKind::Literal(LiteralValue::Integer(value)),
            ty: Ty::int(crate::ty::IntBits::I64, span.clone()),
            span,
        }
    }

    /// Create a float literal expression.
    pub fn float(value: f64, span: Span) -> Self {
        Expression {
            kind: ExprKind::Literal(LiteralValue::Float(value)),
            ty: Ty::float(crate::ty::FloatBits::F64, span.clone()),
            span,
        }
    }

    /// Create a string literal expression.
    pub fn string(value: String, span: Span) -> Self {
        Expression {
            kind: ExprKind::Literal(LiteralValue::String(value)),
            ty: Ty::string(span.clone()),
            span,
        }
    }

    /// Create a boolean literal expression.
    pub fn bool(value: bool, span: Span) -> Self {
        Expression {
            kind: ExprKind::Literal(LiteralValue::Bool(value)),
            ty: Ty::bool(span.clone()),
            span,
        }
    }

    /// Create an array literal expression.
    pub fn array(elements: Vec<Expression>, element_ty: Ty, span: Span) -> Self {
        let ty = Ty::array(element_ty, span.clone());
        Expression {
            kind: ExprKind::Array(elements),
            ty,
            span,
        }
    }

    /// Create a tuple literal expression.
    pub fn tuple(elements: Vec<Expression>, span: Span) -> Self {
        let element_types: Vec<Ty> = elements.iter().map(|e| e.ty.clone()).collect();
        let ty = Ty::tuple(element_types, span.clone());
        Expression {
            kind: ExprKind::Tuple(elements),
            ty,
            span,
        }
    }

    /// Create a grouping expression.
    pub fn grouping(inner: Expression, span: Span) -> Self {
        let ty = inner.ty.clone();
        Expression {
            kind: ExprKind::Grouping(Box::new(inner)),
            ty,
            span,
        }
    }

    /// Create a local variable reference expression.
    pub fn local_ref(local_id: LocalId, ty: Ty, span: Span) -> Self {
        Expression {
            kind: ExprKind::LocalRef(local_id),
            ty,
            span,
        }
    }

    /// Create a symbol reference expression.
    pub fn symbol_ref(symbol_id: SymbolId, ty: Ty, span: Span) -> Self {
        Expression {
            kind: ExprKind::SymbolRef(symbol_id),
            ty,
            span,
        }
    }

    /// Create an overloaded function reference expression.
    /// Type is inferred later when call resolution disambiguates the overload.
    pub fn overloaded_ref(candidates: Vec<SymbolId>, span: Span) -> Self {
        Expression {
            kind: ExprKind::OverloadedRef(candidates),
            ty: Ty::inferred(span.clone()),
            span,
        }
    }

    /// Create a field access expression.
    pub fn field_access(object: Expression, field: String, ty: Ty, span: Span) -> Self {
        Expression {
            kind: ExprKind::FieldAccess {
                object: Box::new(object),
                field,
            },
            ty,
            span,
        }
    }

    /// Create an error expression (poison value).
    pub fn error(span: Span) -> Self {
        Expression {
            kind: ExprKind::Error,
            ty: Ty::error(span.clone()),
            span,
        }
    }

    /// Check if this is a literal expression.
    pub fn is_literal(&self) -> bool {
        matches!(self.kind, ExprKind::Literal(_))
    }

    /// Check if this is an error expression.
    pub fn is_error(&self) -> bool {
        matches!(self.kind, ExprKind::Error)
    }

    /// Get the literal value if this is a literal expression.
    pub fn as_literal(&self) -> Option<&LiteralValue> {
        match &self.kind {
            ExprKind::Literal(val) => Some(val),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_literal() {
        let expr = Expression::integer(42, 0..2);
        assert!(expr.is_literal());
        assert_eq!(expr.as_literal(), Some(&LiteralValue::Integer(42)));
        assert!(expr.ty.is_int());
    }

    #[test]
    fn test_string_literal() {
        let expr = Expression::string("hello".to_string(), 0..7);
        assert!(expr.is_literal());
        assert_eq!(
            expr.as_literal(),
            Some(&LiteralValue::String("hello".to_string()))
        );
        assert!(expr.ty.is_string());
    }

    #[test]
    fn test_tuple_expression() {
        let elements = vec![Expression::integer(1, 1..2), Expression::integer(2, 4..5)];
        let expr = Expression::tuple(elements, 0..6);
        assert!(expr.ty.is_tuple());
    }

    #[test]
    fn test_error_expression() {
        let expr = Expression::error(0..5);
        assert!(expr.is_error());
        assert!(expr.ty.is_error());
    }
}
