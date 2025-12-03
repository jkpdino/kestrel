//! Tests for expression and statement data types.
//!
//! These tests exercise the new Expression, Statement, and CodeBlock types
//! to ensure they work correctly with the semantic tree.

use kestrel_test_suite::*;

mod expression_types {
    #[test]
    fn expression_struct_creation() {
        use kestrel_semantic_tree::expr::{Expression, ExprKind, LiteralValue};

        // Test creating various expression types directly
        let int_expr = Expression::integer(42, 0..2);
        assert!(matches!(int_expr.kind, ExprKind::Literal(LiteralValue::Integer(42))));
        assert!(int_expr.ty.is_int());

        let float_expr = Expression::float(3.14, 0..4);
        assert!(matches!(int_expr.kind, ExprKind::Literal(LiteralValue::Integer(_))));
        assert!(float_expr.ty.is_float());

        let string_expr = Expression::string("hello".to_string(), 0..7);
        assert!(string_expr.ty.is_string());

        let bool_expr = Expression::bool(true, 0..4);
        assert!(bool_expr.ty.is_bool());

        let unit_expr = Expression::unit(0..2);
        assert!(unit_expr.ty.is_unit());
    }

    #[test]
    fn expression_array_creation() {
        use kestrel_semantic_tree::expr::{Expression, ExprKind};
        use kestrel_semantic_tree::ty::{Ty, IntBits};

        let elements = vec![
            Expression::integer(1, 1..2),
            Expression::integer(2, 4..5),
            Expression::integer(3, 7..8),
        ];
        let element_ty = Ty::int(IntBits::I64, 0..0);
        let array_expr = Expression::array(elements, element_ty, 0..10);

        assert!(matches!(array_expr.kind, ExprKind::Array(_)));
        assert!(array_expr.ty.is_array());
    }

    #[test]
    fn expression_tuple_creation() {
        use kestrel_semantic_tree::expr::{Expression, ExprKind};

        let elements = vec![
            Expression::integer(1, 1..2),
            Expression::string("hello".to_string(), 4..11),
        ];
        let tuple_expr = Expression::tuple(elements, 0..12);

        assert!(matches!(tuple_expr.kind, ExprKind::Tuple(_)));
        assert!(tuple_expr.ty.is_tuple());
    }

    #[test]
    fn expression_grouping_creation() {
        use kestrel_semantic_tree::expr::{Expression, ExprKind};

        let inner = Expression::integer(42, 1..3);
        let grouped = Expression::grouping(inner, 0..4);

        assert!(matches!(grouped.kind, ExprKind::Grouping(_)));
        // Grouping should preserve the inner type
        assert!(grouped.ty.is_int());
    }

    #[test]
    fn expression_local_ref() {
        use kestrel_semantic_tree::expr::{Expression, ExprKind};
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{Ty, IntBits};

        let local_id = LocalId(0);
        let ty = Ty::int(IntBits::I64, 0..3);
        let local_ref = Expression::local_ref(local_id, ty, 0..1);

        assert!(matches!(local_ref.kind, ExprKind::LocalRef(id) if id == LocalId(0)));
    }

    #[test]
    fn expression_symbol_ref() {
        use kestrel_semantic_tree::expr::{Expression, ExprKind};
        use kestrel_semantic_tree::ty::{Ty, IntBits};
        use semantic_tree::symbol::SymbolId;

        let symbol_id = SymbolId::new();
        let ty = Ty::int(IntBits::I64, 0..3);
        let symbol_ref = Expression::symbol_ref(symbol_id, ty, 0..5);

        assert!(matches!(symbol_ref.kind, ExprKind::SymbolRef(_)));
    }

    #[test]
    fn expression_overloaded_ref() {
        use kestrel_semantic_tree::expr::{Expression, ExprKind};
        use semantic_tree::symbol::SymbolId;

        let candidates = vec![SymbolId::new(), SymbolId::new(), SymbolId::new()];
        let overloaded = Expression::overloaded_ref(candidates, 0..5);

        assert!(matches!(overloaded.kind, ExprKind::OverloadedRef(ref c) if c.len() == 3));
        // Type should be inferred (unknown until call resolution)
        assert!(overloaded.ty.is_inferred());
    }

    #[test]
    fn expression_error() {
        use kestrel_semantic_tree::expr::Expression;

        let error_expr = Expression::error(0..5);

        assert!(error_expr.is_error());
        assert!(error_expr.ty.is_error());
    }
}

mod statement_types {
    #[test]
    fn statement_let_creation() {
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let pattern = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 10..12),
            0..1,
        );
        let init = Expression::integer(42, 10..12);
        let stmt = Statement::binding(pattern, Some(init), 0..13);

        assert!(stmt.is_binding());
        assert!(!stmt.is_expr());
        assert_eq!(stmt.pattern().and_then(|p| p.local_id()), Some(LocalId(0)));
        assert_eq!(stmt.pattern().and_then(|p| p.mutability()), Some(Mutability::Immutable));
    }

    #[test]
    fn statement_var_creation() {
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::Ty;

        let pattern = Pattern::local(
            LocalId(1),
            Mutability::Mutable,
            "y".to_string(),
            Ty::string(10..17),
            0..1,
        );
        let init = Expression::string("hello".to_string(), 10..17);
        let stmt = Statement::binding(pattern, Some(init), 0..18);

        assert!(stmt.is_binding());
        assert!(!stmt.is_expr());
        assert_eq!(stmt.pattern().and_then(|p| p.local_id()), Some(LocalId(1)));
        assert_eq!(stmt.pattern().and_then(|p| p.mutability()), Some(Mutability::Mutable));
    }

    #[test]
    fn statement_without_initializer() {
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let pattern = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 0..3),
            0..1,
        );
        let stmt = Statement::binding(pattern, None, 0..10);

        assert!(stmt.is_binding());
        assert_eq!(stmt.pattern().and_then(|p| p.local_id()), Some(LocalId(0)));
    }

    #[test]
    fn statement_expr_creation() {
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::stmt::Statement;

        let expr = Expression::unit(0..2);
        let stmt = Statement::expr(expr, 0..3);

        assert!(!stmt.is_binding());
        assert!(stmt.is_expr());
        assert!(stmt.pattern().is_none());
    }

    #[test]
    fn statement_span() {
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let pattern = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 0..3),
            0..1,
        );
        let stmt = Statement::binding(pattern, None, 5..15);
        assert_eq!(stmt.span, 5..15);
    }
}

mod code_block_types {
    #[test]
    fn code_block_empty() {
        use kestrel_semantic_tree::behavior::executable::CodeBlock;

        let block = CodeBlock::empty();

        assert!(block.is_empty());
        assert!(block.statements.is_empty());
        assert!(block.yield_expr().is_none());
    }

    #[test]
    fn code_block_with_statements() {
        use kestrel_semantic_tree::behavior::executable::CodeBlock;
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let pattern1 = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 0..1),
            0..1,
        );
        let pattern2 = Pattern::local(
            LocalId(1),
            Mutability::Immutable,
            "y".to_string(),
            Ty::int(IntBits::I64, 11..12),
            11..12,
        );
        let stmt1 = Statement::binding(pattern1, Some(Expression::integer(1, 0..1)), 0..10);
        let stmt2 = Statement::binding(pattern2, Some(Expression::integer(2, 11..12)), 11..21);

        let block = CodeBlock::new(vec![stmt1, stmt2], None);

        assert!(!block.is_empty());
        assert_eq!(block.statements.len(), 2);
        assert!(block.yield_expr().is_none());
    }

    #[test]
    fn code_block_with_yield() {
        use kestrel_semantic_tree::behavior::executable::CodeBlock;
        use kestrel_semantic_tree::expr::Expression;

        let yield_expr = Expression::integer(42, 0..2);
        let block = CodeBlock::new(vec![], Some(yield_expr));

        assert!(!block.is_empty());
        assert!(block.statements.is_empty());
        assert!(block.yield_expr().is_some());
    }

    #[test]
    fn code_block_with_statements_and_yield() {
        use kestrel_semantic_tree::behavior::executable::CodeBlock;
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let pattern = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 0..1),
            0..1,
        );
        let stmt = Statement::binding(pattern, Some(Expression::integer(1, 0..1)), 0..10);
        let yield_expr = Expression::integer(42, 11..13);

        let block = CodeBlock::new(vec![stmt], Some(yield_expr));

        assert!(!block.is_empty());
        assert_eq!(block.statements.len(), 1);
        assert!(block.yield_expr().is_some());
    }
}

mod executable_behavior {
    #[test]
    fn executable_behavior_creation() {
        use kestrel_semantic_tree::behavior::executable::{CodeBlock, ExecutableBehavior};
        use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
        use semantic_tree::behavior::Behavior;

        let block = CodeBlock::empty();
        let behavior = ExecutableBehavior::new(block);

        assert_eq!(behavior.kind(), KestrelBehaviorKind::Executable);
        assert!(behavior.body().is_empty());
    }

    #[test]
    fn executable_behavior_with_body() {
        use kestrel_semantic_tree::behavior::executable::{CodeBlock, ExecutableBehavior};
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let pattern = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 0..1),
            0..1,
        );
        let stmt = Statement::binding(pattern, Some(Expression::integer(1, 0..1)), 0..10);
        let yield_expr = Expression::integer(42, 11..13);
        let block = CodeBlock::new(vec![stmt], Some(yield_expr));

        let behavior = ExecutableBehavior::new(block);

        assert_eq!(behavior.body().statements.len(), 1);
        assert!(behavior.body().yield_expr().is_some());
    }

    #[test]
    fn executable_behavior_mutable_body() {
        use kestrel_semantic_tree::behavior::executable::{CodeBlock, ExecutableBehavior};
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let block = CodeBlock::empty();
        let mut behavior = ExecutableBehavior::new(block);

        // Initially empty
        assert!(behavior.body().is_empty());

        // Add a statement via mutable access
        let pattern = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 0..1),
            0..1,
        );
        let stmt = Statement::binding(pattern, Some(Expression::integer(1, 0..1)), 0..10);
        behavior.body_mut().statements.push(stmt);

        assert_eq!(behavior.body().statements.len(), 1);
    }
}

mod literal_value_equality {
    #[test]
    fn literal_values_equal() {
        use kestrel_semantic_tree::expr::LiteralValue;

        assert_eq!(LiteralValue::Unit, LiteralValue::Unit);
        assert_eq!(LiteralValue::Integer(42), LiteralValue::Integer(42));
        assert_eq!(LiteralValue::Float(3.14), LiteralValue::Float(3.14));
        assert_eq!(
            LiteralValue::String("hello".to_string()),
            LiteralValue::String("hello".to_string())
        );
        assert_eq!(LiteralValue::Bool(true), LiteralValue::Bool(true));
    }

    #[test]
    fn literal_values_not_equal() {
        use kestrel_semantic_tree::expr::LiteralValue;

        assert_ne!(LiteralValue::Integer(42), LiteralValue::Integer(43));
        assert_ne!(LiteralValue::Bool(true), LiteralValue::Bool(false));
        assert_ne!(
            LiteralValue::String("hello".to_string()),
            LiteralValue::String("world".to_string())
        );
    }
}

mod cloning {
    #[test]
    fn expression_clone() {
        use kestrel_semantic_tree::expr::Expression;

        let expr = Expression::integer(42, 0..2);
        let cloned = expr.clone();

        assert!(cloned.ty.is_int());
        assert_eq!(cloned.span, 0..2);
    }

    #[test]
    fn statement_clone() {
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let pattern = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 0..2),
            0..1,
        );
        let stmt = Statement::binding(pattern, Some(Expression::integer(42, 0..2)), 0..10);
        let cloned = stmt.clone();

        assert!(cloned.is_binding());
        assert_eq!(cloned.pattern().and_then(|p| p.local_id()), Some(LocalId(0)));
    }

    #[test]
    fn code_block_clone() {
        use kestrel_semantic_tree::behavior::executable::CodeBlock;
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::pattern::{Mutability, Pattern};
        use kestrel_semantic_tree::stmt::Statement;
        use kestrel_semantic_tree::symbol::local::LocalId;
        use kestrel_semantic_tree::ty::{IntBits, Ty};

        let pattern = Pattern::local(
            LocalId(0),
            Mutability::Immutable,
            "x".to_string(),
            Ty::int(IntBits::I64, 0..1),
            0..1,
        );
        let stmt = Statement::binding(pattern, Some(Expression::integer(1, 0..1)), 0..10);
        let block = CodeBlock::new(vec![stmt], Some(Expression::integer(42, 11..13)));
        let cloned = block.clone();

        assert_eq!(cloned.statements.len(), 1);
        assert!(cloned.yield_expr().is_some());
    }

    #[test]
    fn executable_behavior_clone() {
        use kestrel_semantic_tree::behavior::executable::{CodeBlock, ExecutableBehavior};

        let block = CodeBlock::empty();
        let behavior = ExecutableBehavior::new(block);
        let cloned = behavior.clone();

        assert!(cloned.body().is_empty());
    }
}

mod nested_expressions {
    #[test]
    fn deeply_nested_array() {
        use kestrel_semantic_tree::expr::Expression;
        use kestrel_semantic_tree::ty::{Ty, IntBits};

        // Create [[1, 2], [3, 4]]
        let inner1 = vec![
            Expression::integer(1, 0..1),
            Expression::integer(2, 3..4),
        ];
        let inner2 = vec![
            Expression::integer(3, 0..1),
            Expression::integer(4, 3..4),
        ];

        let element_ty = Ty::int(IntBits::I64, 0..0);
        let array1 = Expression::array(inner1, element_ty.clone(), 0..5);
        let array2 = Expression::array(inner2, element_ty.clone(), 7..12);

        let outer_element_ty = Ty::array(element_ty, 0..0);
        let outer = Expression::array(vec![array1, array2], outer_element_ty, 0..13);

        assert!(outer.ty.is_array());
    }

    #[test]
    fn nested_tuple_in_array() {
        use kestrel_semantic_tree::expr::Expression;

        // Create [(1, "a"), (2, "b")]
        let tuple1 = Expression::tuple(
            vec![
                Expression::integer(1, 1..2),
                Expression::string("a".to_string(), 4..7),
            ],
            0..8,
        );
        let tuple2 = Expression::tuple(
            vec![
                Expression::integer(2, 1..2),
                Expression::string("b".to_string(), 4..7),
            ],
            10..18,
        );

        let element_ty = tuple1.ty.clone();
        let array = Expression::array(vec![tuple1, tuple2], element_ty, 0..20);

        assert!(array.ty.is_array());
    }

    #[test]
    fn nested_grouping() {
        use kestrel_semantic_tree::expr::Expression;

        // Create (((42)))
        let inner = Expression::integer(42, 3..5);
        let g1 = Expression::grouping(inner, 2..6);
        let g2 = Expression::grouping(g1, 1..7);
        let g3 = Expression::grouping(g2, 0..8);

        // All groupings should preserve the Int type
        assert!(g3.ty.is_int());
    }
}

/// Integration tests that compile actual Kestrel code
mod integration {
    use super::*;

    #[test]
    fn function_with_integer_body() {
        Test::new(
            r#"module Test
            func answer() -> Int { 42 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_string_body() {
        Test::new(
            r#"module Test
            func greeting() -> String { "hello" }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_tuple_body() {
        Test::new(
            r#"module Test
            func pair() { (1, 2) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_array_body() {
        Test::new(
            r#"module Test
            func numbers() { [1, 2, 3] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_nested_expressions() {
        Test::new(
            r#"module Test
            func complex() { [(1, 2), (3, 4)] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_unit_body() {
        Test::new(
            r#"module Test
            func nothing() { () }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_bool_body() {
        Test::new(
            r#"module Test
            func yes() { true }
            func no() { false }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_functions_with_bodies() {
        Test::new(
            r#"module Test
            func one() { 1 }
            func two() { 2 }
            func three() { 3 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn struct_with_method_body() {
        Test::new(
            r#"module Test
            struct Point {
                func origin() { (0, 0) }
            }
        "#,
        )
        .expect(Compiles);
    }
}
