//! Expression data types for the semantic tree.
//!
//! Expressions are plain data structures (not symbols) that represent
//! resolved expressions in function bodies. They are created during
//! the bind phase after path resolution.

use kestrel_span::Span;
use semantic_tree::symbol::SymbolId;

use crate::symbol::local::LocalId;
use crate::ty::Ty;

/// A call argument with optional label.
///
/// Supports Swift-style labeled arguments:
/// - `foo(42)` → label = None
/// - `foo(x: 42)` → label = Some("x")
#[derive(Debug, Clone)]
pub struct CallArgument {
    /// Optional label for the argument
    pub label: Option<String>,
    /// The argument value expression
    pub value: Expression,
    /// The span of the entire argument (including label if present)
    pub span: Span,
}

impl CallArgument {
    /// Create an unlabeled argument.
    pub fn unlabeled(value: Expression, span: Span) -> Self {
        Self {
            label: None,
            value,
            span,
        }
    }

    /// Create a labeled argument.
    pub fn labeled(label: String, value: Expression, span: Span) -> Self {
        Self {
            label: Some(label),
            value,
            span,
        }
    }
}

/// Built-in methods on primitive types.
///
/// These methods have no symbol representation - the compiler has
/// built-in knowledge of their signatures and semantics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveMethod {
    // Int methods
    /// Int.toString() -> String
    IntToString,
    /// Int.abs() -> Int
    IntAbs,

    // String methods
    /// String.length() -> Int
    StringLength,
    /// String.isEmpty() -> Bool
    StringIsEmpty,

    // Future: more methods as needed
}

impl PrimitiveMethod {
    /// Get the return type of this primitive method.
    pub fn return_type(&self, span: Span) -> Ty {
        use crate::ty::IntBits;
        match self {
            PrimitiveMethod::IntToString => Ty::string(span),
            PrimitiveMethod::IntAbs => Ty::int(IntBits::I64, span),
            PrimitiveMethod::StringLength => Ty::int(IntBits::I64, span),
            PrimitiveMethod::StringIsEmpty => Ty::bool(span),
        }
    }

    /// Get the method name.
    pub fn name(&self) -> &'static str {
        match self {
            PrimitiveMethod::IntToString => "toString",
            PrimitiveMethod::IntAbs => "abs",
            PrimitiveMethod::StringLength => "length",
            PrimitiveMethod::StringIsEmpty => "isEmpty",
        }
    }

    /// Look up a method on a primitive type.
    pub fn lookup(ty: &Ty, name: &str) -> Option<PrimitiveMethod> {
        use crate::ty::TyKind;
        match ty.kind() {
            TyKind::Int(_) => match name {
                "toString" => Some(PrimitiveMethod::IntToString),
                "abs" => Some(PrimitiveMethod::IntAbs),
                _ => None,
            },
            TyKind::String => match name {
                "length" => Some(PrimitiveMethod::StringLength),
                "isEmpty" => Some(PrimitiveMethod::StringIsEmpty),
                _ => None,
            },
            _ => None,
        }
    }
}

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
    /// Reference to a type (struct, protocol, etc.) - used in call expressions.
    /// When calling a type name like `Point(x: 1, y: 2)`, this represents the struct.
    TypeRef(SymbolId),

    // Member access
    /// Field access: `obj.field`
    /// The object expression and the field name.
    FieldAccess {
        object: Box<Expression>,
        field: String,
    },

    // Method references
    /// Method reference: `receiver.method`
    /// Represents a method lookup on a receiver before being called.
    /// The candidates list may have multiple entries for overloaded methods.
    MethodRef {
        receiver: Box<Expression>,
        candidates: Vec<SymbolId>,
        method_name: String,
    },

    // Calls
    /// Function or method call: `foo(1, 2)` or `obj.method(1, 2)`
    /// The callee can be SymbolRef, MethodRef, OverloadedRef, or any callable expression.
    Call {
        callee: Box<Expression>,
        arguments: Vec<CallArgument>,
    },

    /// Primitive method call: `5.toString()`, `"hello".length()`
    /// No symbol exists - compiler has built-in knowledge.
    PrimitiveMethodCall {
        receiver: Box<Expression>,
        method: PrimitiveMethod,
        arguments: Vec<CallArgument>,
    },

    /// Implicit struct initialization: `Point(x: 1, y: 2)` when no explicit init exists.
    /// The compiler generates a memberwise initializer that assigns each argument to
    /// the corresponding field in declaration order.
    ///
    /// This is used when:
    /// - The struct has no explicit initializers
    /// - All fields are visible from the call site
    ImplicitStructInit {
        /// The struct type being initialized
        struct_type: Ty,
        /// Arguments matching struct fields in declaration order
        arguments: Vec<CallArgument>,
    },

    /// Assignment expression: target = value
    /// Type is Never (assignment doesn't produce a usable value)
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },

    // Future: If, While, Block, BinaryOp, UnaryOp, etc.

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

    /// Create a type reference expression.
    /// Used when a path resolves to a type (e.g., struct name in `Point(x: 1, y: 2)`).
    pub fn type_ref(symbol_id: SymbolId, ty: Ty, span: Span) -> Self {
        Expression {
            kind: ExprKind::TypeRef(symbol_id),
            ty,
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

    /// Create a method reference expression.
    /// Type is inferred later when call resolution happens.
    pub fn method_ref(
        receiver: Expression,
        candidates: Vec<SymbolId>,
        method_name: String,
        span: Span,
    ) -> Self {
        Expression {
            kind: ExprKind::MethodRef {
                receiver: Box::new(receiver),
                candidates,
                method_name,
            },
            ty: Ty::inferred(span.clone()),
            span,
        }
    }

    /// Create a call expression.
    pub fn call(callee: Expression, arguments: Vec<CallArgument>, return_ty: Ty, span: Span) -> Self {
        Expression {
            kind: ExprKind::Call {
                callee: Box::new(callee),
                arguments,
            },
            ty: return_ty,
            span,
        }
    }

    /// Create a primitive method call expression.
    pub fn primitive_method_call(
        receiver: Expression,
        method: PrimitiveMethod,
        arguments: Vec<CallArgument>,
        span: Span,
    ) -> Self {
        let return_ty = method.return_type(span.clone());
        Expression {
            kind: ExprKind::PrimitiveMethodCall {
                receiver: Box::new(receiver),
                method,
                arguments,
            },
            ty: return_ty,
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

    /// Create an implicit struct initialization expression.
    ///
    /// Used when calling a struct type without explicit initializers.
    pub fn implicit_struct_init(struct_type: Ty, arguments: Vec<CallArgument>, span: Span) -> Self {
        Expression {
            kind: ExprKind::ImplicitStructInit {
                struct_type: struct_type.clone(),
                arguments,
            },
            ty: struct_type,
            span,
        }
    }

    /// Create an assignment expression.
    ///
    /// The type of an assignment expression is Never, meaning the value
    /// cannot be used. This prevents chaining like `x = y = z`.
    pub fn assignment(target: Expression, value: Expression, span: Span) -> Self {
        Expression {
            kind: ExprKind::Assignment {
                target: Box::new(target),
                value: Box::new(value),
            },
            ty: Ty::never(span.clone()),
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
