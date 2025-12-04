//! Tests for control flow expressions (if/else).
//!
//! These tests verify that:
//! - If expressions parse correctly
//! - Else branches work
//! - Else-if chaining works
//! - Variables in if blocks have proper scoping (not visible outside)
//! - If expressions can be used as values

use kestrel_test_suite::*;

mod if_basic {
    use super::*;

    #[test]
    fn if_without_else_compiles() {
        Test::new(
            r#"
module Main

func test() {
    if true {
        1
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn if_without_semicolon_followed_by_expression() {
        // If expressions don't need semicolons - they are statement-like
        Test::new(
            r#"
module Main

func test() -> Int {
    if false {
        1
    }
    42
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn multiple_if_without_semicolons() {
        Test::new(
            r#"
module Main

func test() -> Int {
    if false {
        1
    }
    if false {
        2
    }
    if true {
        3
    } else {
        4
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn if_with_else_compiles() {
        Test::new(
            r#"
module Main

func test() -> Int {
    if true {
        1
    } else {
        2
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn if_else_if_chain_compiles() {
        Test::new(
            r#"
module Main

func test(x: Int) -> Int {
    if x == 1 {
        10
    } else if x == 2 {
        20
    } else if x == 3 {
        30
    } else {
        0
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn if_with_complex_condition() {
        Test::new(
            r#"
module Main

func test(a: Bool, b: Bool) -> Int {
    if a and b {
        1
    } else {
        0
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn nested_if_expressions() {
        Test::new(
            r#"
module Main

func test(a: Bool, b: Bool) -> Int {
    if a {
        if b {
            1
        } else {
            2
        }
    } else {
        3
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }
}

mod if_scoping {
    use super::*;

    #[test]
    fn variable_in_if_block_not_visible_outside() {
        Test::new(
            r#"
module Main

func test() -> Int {
    if true {
        let x: Int = 42;
        x
    }
    x
}
"#,
        )
        .expect(Fails)
        .expect(HasError("undefined"));
    }

    #[test]
    fn variable_in_else_block_not_visible_outside() {
        Test::new(
            r#"
module Main

func test() -> Int {
    if true {
        1
    } else {
        let y: Int = 10;
        y
    }
    y
}
"#,
        )
        .expect(Fails)
        .expect(HasError("undefined"));
    }

    #[test]
    fn variable_visible_within_its_block() {
        Test::new(
            r#"
module Main

func test() -> Int {
    if true {
        let x: Int = 5;
        let y: Int = x + 1;
        y
    } else {
        0
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn outer_variable_visible_inside_if() {
        Test::new(
            r#"
module Main

func test() -> Int {
    let outer: Int = 10;
    if true {
        outer + 5
    } else {
        outer - 5
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn shadowing_inside_if_block() {
        Test::new(
            r#"
module Main

func test() -> Int {
    let x: Int = 100;
    if true {
        let x: Int = 1;
        x
    } else {
        x
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }
}

mod if_with_statements {
    use super::*;

    #[test]
    fn if_block_with_multiple_statements() {
        Test::new(
            r#"
module Main

func test() -> Int {
    if true {
        let a: Int = 1;
        let b: Int = 2;
        let c: Int = a + b;
        c
    } else {
        0
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }

    #[test]
    fn if_block_with_side_effects() {
        // Test that we can have side effects (assignments) in an if block
        Test::new(
            r#"
module Main

func test() {
    var localX: Int = 0;
    if true {
        localX = 10;
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function));
    }
}
