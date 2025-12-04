use kestrel_test_suite::*;

mod integers {
    use super::*;

    #[test]
    fn integer_decimal() {
        Test::new(
            r#"module Test
            func test() { 42 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn integer_hex() {
        Test::new(
            r#"module Test
            func test() { 0xFF }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn integer_hex_uppercase() {
        Test::new(
            r#"module Test
            func test() { 0XAB }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn integer_binary() {
        Test::new(
            r#"module Test
            func test() { 0b1010 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn integer_octal() {
        Test::new(
            r#"module Test
            func test() { 0o755 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn integer_zero() {
        Test::new(
            r#"module Test
            func test() { 0 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn integer_large() {
        Test::new(
            r#"module Test
            func test() { 9223372036854775807 }
        "#,
        )
        .expect(Compiles);
    }
}

mod floats {
    use super::*;

    #[test]
    fn float_simple() {
        Test::new(
            r#"module Test
            func test() { 3.14 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn float_scientific() {
        Test::new(
            r#"module Test
            func test() { 1.0e10 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn float_scientific_negative() {
        Test::new(
            r#"module Test
            func test() { 2.5E-3 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn float_scientific_positive() {
        Test::new(
            r#"module Test
            func test() { 1.0e+5 }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn float_zero() {
        Test::new(
            r#"module Test
            func test() { 0.0 }
        "#,
        )
        .expect(Compiles);
    }
}

mod strings {
    use super::*;

    #[test]
    fn string_simple() {
        Test::new(
            r#"module Test
            func test() { "hello" }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_empty() {
        Test::new(
            r#"module Test
            func test() { "" }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_with_escape_newline() {
        Test::new(
            r#"module Test
            func test() { "hello\nworld" }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_with_escape_tab() {
        Test::new(
            r#"module Test
            func test() { "hello\tworld" }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_with_escape_quote() {
        Test::new(
            r#"module Test
            func test() { "say \"hello\"" }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_with_escape_backslash() {
        Test::new(
            r#"module Test
            func test() { "path\\to\\file" }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_with_spaces() {
        Test::new(
            r#"module Test
            func test() { "hello world" }
        "#,
        )
        .expect(Compiles);
    }
}

mod booleans {
    use super::*;

    #[test]
    fn bool_true() {
        Test::new(
            r#"module Test
            func test() { true }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn bool_false() {
        Test::new(
            r#"module Test
            func test() { false }
        "#,
        )
        .expect(Compiles);
    }
}

mod arrays {
    use super::*;

    #[test]
    fn array_empty() {
        Test::new(
            r#"module Test
            func test() { [] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_single() {
        Test::new(
            r#"module Test
            func test() { [1] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_multiple() {
        Test::new(
            r#"module Test
            func test() { [1, 2, 3] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_trailing_comma() {
        Test::new(
            r#"module Test
            func test() { [1, 2, 3,] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_nested() {
        Test::new(
            r#"module Test
            func test() { [[1, 2], [3, 4]] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_of_strings() {
        Test::new(
            r#"module Test
            func test() { ["hello", "world"] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_of_booleans() {
        Test::new(
            r#"module Test
            func test() { [true, false, true] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_mixed_types() {
        // Type checking will catch this later; parser should accept it
        Test::new(
            r#"module Test
            func test() { [1, "hello", true] }
        "#,
        )
        .expect(Compiles);
    }
}

mod tuples {
    use super::*;

    #[test]
    fn tuple_single_element() {
        // Single element with trailing comma is a tuple
        Test::new(
            r#"module Test
            func test() { (1,) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_two_elements() {
        Test::new(
            r#"module Test
            func test() { (1, 2) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_multiple() {
        Test::new(
            r#"module Test
            func test() { (1, 2, 3) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_trailing_comma() {
        Test::new(
            r#"module Test
            func test() { (1, 2, 3,) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_nested() {
        Test::new(
            r#"module Test
            func test() { ((1, 2), (3, 4)) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_mixed_types() {
        Test::new(
            r#"module Test
            func test() { (1, "hello", true) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_of_arrays() {
        Test::new(
            r#"module Test
            func test() { ([1, 2], [3, 4]) }
        "#,
        )
        .expect(Compiles);
    }
}

mod grouping {
    use super::*;

    #[test]
    fn grouping_integer() {
        // Single element without trailing comma is grouping
        Test::new(
            r#"module Test
            func test() { (42) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn grouping_nested() {
        Test::new(
            r#"module Test
            func test() { ((42)) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn grouping_string() {
        Test::new(
            r#"module Test
            func test() { ("hello") }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn grouping_array() {
        Test::new(
            r#"module Test
            func test() { ([1, 2, 3]) }
        "#,
        )
        .expect(Compiles);
    }
}

mod unit {
    use super::*;

    #[test]
    fn unit_expression() {
        Test::new(
            r#"module Test
            func test() { () }
        "#,
        )
        .expect(Compiles);
    }
}

mod complex {
    use super::*;

    #[test]
    fn array_of_tuples() {
        Test::new(
            r#"module Test
            func test() { [(1, 2), (3, 4)] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested() {
        Test::new(
            r#"module Test
            func test() { [[(1,)]] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn mixed_literals_in_function() {
        Test::new(
            r#"module Test
            func test() { 42 }
            func test2() { 3.14 }
            func test3() { "hello" }
            func test4() { true }
            func test5() { [1, 2, 3] }
            func test6() { (1, 2) }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("test").is(SymbolKind::Function))
        .expect(Symbol::new("test2").is(SymbolKind::Function))
        .expect(Symbol::new("test3").is(SymbolKind::Function))
        .expect(Symbol::new("test4").is(SymbolKind::Function))
        .expect(Symbol::new("test5").is(SymbolKind::Function))
        .expect(Symbol::new("test6").is(SymbolKind::Function));
    }
}
