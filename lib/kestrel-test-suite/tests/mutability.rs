//! Integration tests for mutability validation.
//!
//! Tests that assignment to immutable variables and fields is properly rejected.

use kestrel_test_suite::*;

mod local_variables {
    use super::*;

    #[test]
    fn assign_to_var_succeeds() {
        Test::new(
            r#"module Test
            func test() -> Int {
                var x: Int = 5;
                x = 10;
                x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn assign_to_let_fails() {
        Test::new(
            r#"module Test
            func test() -> Int {
                let x: Int = 5;
                x = 10;
                x
            }
        "#,
        )
        .expect(HasError("cannot assign to immutable variable"));
    }

    #[test]
    fn assign_to_parameter_fails() {
        // Parameters are immutable by default
        Test::new(
            r#"module Test
            func test(x: Int) -> Int {
                x = 10;
                x
            }
        "#,
        )
        .expect(HasError("cannot assign to immutable"));
    }

    #[test]
    fn multiple_assignments_to_var() {
        Test::new(
            r#"module Test
            func test() -> Int {
                var x: Int = 1;
                x = 2;
                x = 3;
                x = 4;
                x
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod field_access {
    use super::*;

    #[test]
    fn assign_to_var_field_on_var_receiver() {
        Test::new(
            r#"module Test
            struct Point {
                var x: Int
                var y: Int
            }
            func test() -> Int {
                var p = Point(x: 1, y: 2);
                p.x = 10;
                p.x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn assign_to_let_field_fails() {
        Test::new(
            r#"module Test
            struct Point {
                let x: Int
                var y: Int
            }
            func test() -> Int {
                var p = Point(x: 1, y: 2);
                p.x = 10;
                p.x
            }
        "#,
        )
        .expect(HasError("cannot assign to immutable field"));
    }

    #[test]
    fn assign_to_var_field_on_let_receiver_fails() {
        Test::new(
            r#"module Test
            struct Point {
                var x: Int
                var y: Int
            }
            func test() -> Int {
                let p = Point(x: 1, y: 2);
                p.x = 10;
                p.x
            }
        "#,
        )
        .expect(HasError("cannot assign to immutable field"));
    }

    #[test]
    fn assign_to_let_field_on_let_receiver_fails() {
        Test::new(
            r#"module Test
            struct Point {
                let x: Int
                let y: Int
            }
            func test() -> Int {
                let p = Point(x: 1, y: 2);
                p.x = 10;
                p.x
            }
        "#,
        )
        .expect(HasError("cannot assign to immutable field"));
    }

    #[test]
    fn chained_field_access_all_mutable() {
        Test::new(
            r#"module Test
            struct Inner {
                var value: Int
            }
            struct Outer {
                var inner: Inner
            }
            func test() -> Int {
                var o = Outer(inner: Inner(value: 1));
                o.inner.value = 10;
                o.inner.value
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn chained_field_access_inner_let_fails() {
        Test::new(
            r#"module Test
            struct Inner {
                let value: Int
            }
            struct Outer {
                var inner: Inner
            }
            func test() -> Int {
                var o = Outer(inner: Inner(value: 1));
                o.inner.value = 10;
                o.inner.value
            }
        "#,
        )
        .expect(HasError("cannot assign to immutable field"));
    }

    #[test]
    fn chained_field_access_outer_let_fails() {
        Test::new(
            r#"module Test
            struct Inner {
                var value: Int
            }
            struct Outer {
                let inner: Inner
            }
            func test() -> Int {
                var o = Outer(inner: Inner(value: 1));
                o.inner.value = 10;
                o.inner.value
            }
        "#,
        )
        .expect(HasError("cannot assign to immutable field"));
    }

    #[test]
    fn chained_field_access_receiver_let_fails() {
        Test::new(
            r#"module Test
            struct Inner {
                var value: Int
            }
            struct Outer {
                var inner: Inner
            }
            func test() -> Int {
                let o = Outer(inner: Inner(value: 1));
                o.inner.value = 10;
                o.inner.value
            }
        "#,
        )
        .expect(HasError("cannot assign to immutable field"));
    }
}

mod initializers {
    use super::*;

    #[test]
    fn init_can_assign_to_let_field() {
        // In initializers, self.field = value is allowed even for let fields
        Test::new(
            r#"module Test
            struct Point {
                let x: Int
                let y: Int

                init(x: Int, y: Int) {
                    self.x = x
                    self.y = y
                }
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn init_can_assign_to_var_field() {
        Test::new(
            r#"module Test
            struct Point {
                var x: Int
                var y: Int

                init(x: Int, y: Int) {
                    self.x = x
                    self.y = y
                }
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn init_mixed_let_var_fields() {
        Test::new(
            r#"module Test
            struct Mixed {
                let id: Int
                var value: Int

                init(id: Int, value: Int) {
                    self.id = id
                    self.value = value
                }
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod invalid_targets {
    use super::*;

    #[test]
    fn assign_to_literal_fails() {
        Test::new(
            r#"module Test
            func test() -> Int {
                5 = 10;
                0
            }
        "#,
        )
        .expect(HasError("cannot assign to this expression"));
    }

    #[test]
    fn assign_to_call_result_fails() {
        Test::new(
            r#"module Test
            func getValue() -> Int { 5 }
            func test() -> Int {
                getValue() = 10;
                0
            }
        "#,
        )
        .expect(HasError("cannot assign to this expression"));
    }

    #[test]
    fn assign_to_struct_init_fails() {
        Test::new(
            r#"module Test
            struct Point {
                var x: Int
                var y: Int
            }
            func test() -> Int {
                Point(x: 1, y: 2) = Point(x: 3, y: 4);
                0
            }
        "#,
        )
        .expect(HasError("cannot assign to this expression"));
    }
}
