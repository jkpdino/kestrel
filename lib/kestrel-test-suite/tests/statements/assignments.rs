//! Tests for assignment expressions.
//!
//! These tests verify that assignment statements are correctly resolved.

use kestrel_test_suite::*;

mod assignment_expressions {
    use super::*;

    #[test]
    fn assign_to_var() {
        // Basic assignment to a mutable variable
        Test::new(
            r#"
module Main

func test() -> Int {
    var x: Int = 0;
    x = 5;
    x
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn assign_expression_value() {
        // Assign result of function call
        Test::new(
            r#"
module Main

func getValue() -> Int { 42 }

func test() -> Int {
    var x: Int = 0;
    x = getValue();
    x
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn assign_with_complex_rhs() {
        // Assign an array literal
        Test::new(
            r#"
module Main

func test() -> [Int] {
    var arr: [Int] = [];
    arr = [1, 2, 3];
    arr
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_assignments() {
        // Multiple sequential assignments
        Test::new(
            r#"
module Main

func test() -> Int {
    var x: Int = 0;
    x = 1;
    x = 2;
    x = 3;
    x
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn assign_parameter_to_var() {
        // Assign a parameter value to a variable
        Test::new(
            r#"
module Main

func test(value: Int) -> Int {
    var x: Int = 0;
    x = value;
    x
}
"#,
        )
        .expect(Compiles);
    }
}
