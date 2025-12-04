//! Tests for function call expressions.
//!
//! These tests verify that function calls are correctly resolved,
//! including standalone functions, nested calls, and overload resolution.

use kestrel_test_suite::*;

mod function_calls {
    use super::*;

    // === Basic Function Calls ===

    #[test]
    fn call_simple_function() {
        // Call a simple function with no parameters
        Test::new(
            r#"
module Main

func getNumber() -> Int {
    42
}

func test() -> Int {
    getNumber()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_function_with_one_param() {
        // Call a function with one parameter
        Test::new(
            r#"
module Main

func double(x: Int) -> Int {
    42
}

func test() -> Int {
    double(21)
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_function_with_multiple_params() {
        // Call a function with multiple parameters
        Test::new(
            r#"
module Main

func add(x: Int, y: Int) -> Int {
    42
}

func test() -> Int {
    add(1, 2)
}
"#,
        )
        .expect(Compiles);
    }

    // === Labeled Arguments ===

    #[test]
    fn call_with_labeled_argument() {
        // Call a function with a labeled argument
        Test::new(
            r#"
module Main

func greet(with name: String) -> String {
    name
}

func test() -> String {
    greet(with: "world")
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_with_multiple_labeled_arguments() {
        // Call with multiple labeled arguments
        Test::new(
            r#"
module Main

func createPoint(x xVal: Int, y yVal: Int) -> Int {
    42
}

func test() -> Int {
    createPoint(x: 10, y: 20)
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_with_mixed_labeled_unlabeled() {
        // Mix of labeled and unlabeled arguments
        Test::new(
            r#"
module Main

func format(value: Int, with prefix: String) -> String {
    prefix
}

func test() -> String {
    format(42, with: "Result: ")
}
"#,
        )
        .expect(Compiles);
    }

    // === Nested Calls ===

    #[test]
    fn nested_function_calls() {
        // Nested function calls
        Test::new(
            r#"
module Main

func double(x: Int) -> Int {
    42
}

func add(x: Int, y: Int) -> Int {
    42
}

func test() -> Int {
    add(double(1), double(2))
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested_calls() {
        // Deeply nested function calls
        Test::new(
            r#"
module Main

func id(x: Int) -> Int {
    x
}

func test() -> Int {
    id(id(id(id(42))))
}
"#,
        )
        .expect(Compiles);
    }

    // === Error Cases ===

    #[test]
    fn call_with_wrong_arity_error() {
        // BUG FOUND: Wrong number of arguments silently compiles (no error)
        // This should error but currently doesn't
        Test::new(
            r#"
module Main

func add(x: Int, y: Int) -> Int {
    42
}

func test() -> Int {
    add(1)
}
"#,
        )
        .expect(HasError("no matching overload"));
    }

    #[test]
    fn call_undefined_function_error() {
        // Calling undefined function should produce an error
        Test::new(
            r#"
module Main

func test() -> Int {
    undefined()
}
"#,
        )
        .expect(HasError("undefined name"));
    }

    #[test]
    fn call_with_wrong_label_error() {
        // Using wrong label should produce an error
        Test::new(
            r#"
module Main

func greet(with name: String) -> String {
    name
}

func test() -> String {
    greet(using: "world")
}
"#,
        )
        .expect(HasError("no matching overload"));
    }

    // === Return Type Propagation ===

    #[test]
    fn function_return_type_in_expression() {
        // Function return type used in expression
        Test::new(
            r#"
module Main

func getString() -> String {
    "hello"
}

func test() -> String {
    let s: String = getString();
    s
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn void_function_call() {
        // Calling a void function
        Test::new(
            r#"
module Main

func doSomething() -> () {
    ()
}

func test() -> () {
    doSomething()
}
"#,
        )
        .expect(Compiles);
    }
}

