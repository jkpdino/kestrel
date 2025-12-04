//! Tests for literal expressions in function bodies.
//!
//! These tests verify that various literal values (integers, floats, strings, booleans, unit,
//! arrays, tuples) are correctly resolved when used in function bodies.

use kestrel_test_suite::*;

mod literal_expressions {
    use super::*;

    #[test]
    fn integer_literal_in_body() {
        Test::new(
            r#"
module Main

func getValue() -> Int {
    42
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("getValue").is(SymbolKind::Function));
    }

    #[test]
    fn float_literal_in_body() {
        Test::new(
            r#"
module Main

func getValue() -> Float {
    3.14
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_literal_in_body() {
        Test::new(
            r#"
module Main

func getMessage() -> String {
    "hello world"
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn bool_literal_in_body() {
        Test::new(
            r#"
module Main

func isEnabled() -> Bool {
    true
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unit_literal_in_body() {
        Test::new(
            r#"
module Main

func doNothing() -> () {
    ()
}
"#,
        )
        .expect(Compiles);
    }
}

mod composite_expressions {
    use super::*;

    #[test]
    fn array_literal_in_body() {
        Test::new(
            r#"
module Main

func getNumbers() -> [Int] {
    [1, 2, 3]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_literal_in_body() {
        Test::new(
            r#"
module Main

func getPair() -> (Int, String) {
    (42, "hello")
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_tuple_in_body() {
        Test::new(
            r#"
module Main

func getNested() -> ((Int, Int), String) {
    ((1, 2), "point")
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn grouping_expression_in_body() {
        Test::new(
            r#"
module Main

func getValue() -> Int {
    (42)
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_arrays() {
        Test::new(
            r#"
module Main

func getMatrix() -> [[Int]] {
    [[1, 2], [3, 4]]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn mixed_array_elements() {
        Test::new(
            r#"
module Main

func getNumbers() -> [Int] {
    [1, 2, 3, 4, 5]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn empty_tuple_is_unit() {
        Test::new(
            r#"
module Main

func empty() -> () {
    ()
}
"#,
        )
        .expect(Compiles);
    }
}
