//! Tests for exhaustive return analysis
//!
//! These tests verify that functions with non-unit return types
//! return a value on all code paths.

use kestrel_test_suite::*;

mod basic_returns {
    use super::*;

    #[test]
    fn function_with_return_compiles() {
        Test::new(
            r#"
module Main

func test() -> Int {
    return 42
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_value_expression_compiles() {
        Test::new(
            r#"
module Main

func test() -> Int {
    42
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unit_function_no_return_needed() {
        Test::new(
            r#"
module Main

func test() {
    let x: Int = 1;
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_missing_return_fails() {
        Test::new(
            r#"
module Main

func test() -> Int {
    let x: Int = 1;
}
"#,
        )
        .expect(Fails)
        .expect(HasError("does not return a value on all code paths"));
    }
}

mod if_else_branches {
    use super::*;

    #[test]
    fn if_else_both_return() {
        Test::new(
            r#"
module Main

func test(cond: Bool) -> Int {
    if cond {
        return 1
    } else {
        return 2
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn if_else_both_have_values() {
        Test::new(
            r#"
module Main

func test(cond: Bool) -> Int {
    if cond {
        1
    } else {
        2
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn if_without_else_missing_return() {
        Test::new(
            r#"
module Main

func test(cond: Bool) -> Int {
    if cond {
        return 1
    }
}
"#,
        )
        .expect(Fails)
        .expect(HasError("does not return a value on all code paths"));
    }

    #[test]
    fn if_returns_else_falls_through() {
        Test::new(
            r#"
module Main

func test(cond: Bool) -> Int {
    if cond {
        return 1
    } else {
        let x: Int = 2;
    }
}
"#,
        )
        .expect(Fails)
        .expect(HasError("does not return a value on all code paths"));
    }

    #[test]
    fn if_else_chain_all_return() {
        Test::new(
            r#"
module Main

func test(x: Int) -> Int {
    if x == 1 {
        return 10
    } else if x == 2 {
        return 20
    } else {
        return 0
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn if_else_chain_missing_final_else() {
        Test::new(
            r#"
module Main

func test(x: Int) -> Int {
    if x == 1 {
        return 10
    } else if x == 2 {
        return 20
    }
}
"#,
        )
        .expect(Fails)
        .expect(HasError("does not return a value on all code paths"));
    }
}

mod loops {
    use super::*;

    #[test]
    fn infinite_loop_diverges() {
        Test::new(
            r#"
module Main

func test() -> Int {
    loop {
        ()
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn loop_with_return() {
        Test::new(
            r#"
module Main

func test() -> Int {
    loop {
        return 42
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn loop_with_break_needs_return_after() {
        Test::new(
            r#"
module Main

func test() -> Int {
    loop {
        break
    }
}
"#,
        )
        .expect(Fails)
        .expect(HasError("does not return a value on all code paths"));
    }

    #[test]
    fn loop_with_break_and_return_after() {
        Test::new(
            r#"
module Main

func test() -> Int {
    loop {
        break
    }
    42
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn while_loop_may_not_execute() {
        Test::new(
            r#"
module Main

func test(cond: Bool) -> Int {
    while cond {
        return 1
    }
}
"#,
        )
        .expect(Fails)
        .expect(HasError("does not return a value on all code paths"));
    }

    #[test]
    fn while_with_return_after() {
        Test::new(
            r#"
module Main

func test(cond: Bool) -> Int {
    while cond {
        ()
    }
    42
}
"#,
        )
        .expect(Compiles);
    }
}

mod complex_control_flow {
    use super::*;

    #[test]
    fn nested_if_all_paths_return() {
        Test::new(
            r#"
module Main

func test(a: Bool, b: Bool) -> Int {
    if a {
        if b {
            return 1
        } else {
            return 2
        }
    } else {
        return 3
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_if_inner_missing_else() {
        Test::new(
            r#"
module Main

func test(a: Bool, b: Bool) -> Int {
    if a {
        if b {
            return 1
        }
    } else {
        return 3
    }
}
"#,
        )
        .expect(Fails)
        .expect(HasError("does not return a value on all code paths"));
    }

    #[test]
    fn early_return_then_fallthrough() {
        Test::new(
            r#"
module Main

func test(cond: Bool) -> Int {
    if cond {
        return 1
    }
    42
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_early_returns() {
        Test::new(
            r#"
module Main

func test(x: Int) -> Int {
    if x < 0 {
        return -1
    }
    if x == 0 {
        return 0
    }
    1
}
"#,
        )
        .expect(Compiles);
    }
}

mod with_statements {
    use super::*;

    #[test]
    fn statements_then_return() {
        Test::new(
            r#"
module Main

func test() -> Int {
    let x: Int = 1;
    let y: Int = 2;
    return x + y
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn statements_then_value() {
        Test::new(
            r#"
module Main

func test() -> Int {
    let x: Int = 1;
    let y: Int = 2;
    x + y
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn statements_without_return() {
        Test::new(
            r#"
module Main

func test() -> Int {
    let x: Int = 1;
    let y: Int = 2;
}
"#,
        )
        .expect(Fails)
        .expect(HasError("does not return a value on all code paths"));
    }
}
