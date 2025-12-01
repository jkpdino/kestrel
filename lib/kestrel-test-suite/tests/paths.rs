use kestrel_test_suite::*;

mod path_expressions {
    use super::*;

    #[test]
    fn path_single_segment() {
        Test::new(
            r#"module Test
            func test() { foo }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn path_two_segments() {
        Test::new(
            r#"module Test
            func test() { foo.bar }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn path_multiple_segments() {
        Test::new(
            r#"module Test
            func test() { a.b.c.d }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn path_in_array() {
        Test::new(
            r#"module Test
            func test() { [foo, bar] }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn path_in_tuple() {
        Test::new(
            r#"module Test
            func test() { (foo, bar) }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn path_in_grouping() {
        Test::new(
            r#"module Test
            func test() { (foo) }
        "#,
        )
        .expect(Compiles);
    }
}

mod variable_declarations {
    use super::*;

    #[test]
    fn let_with_type() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int = 42;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn var_with_type() {
        Test::new(
            r#"module Test
            func test() {
                var x: Int = 42;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_without_initializer() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_declarations() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int = 1;
                let y: Int = 2;
                let z: Int = 3;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_path_initializer() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int = foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_complex_type() {
        Test::new(
            r#"module Test
            func test() {
                let x: (Int, String) = (1, "hello");
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_array_type() {
        Test::new(
            r#"module Test
            func test() {
                let x: [Int] = [1, 2, 3];
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod variable_shadowing {
    use super::*;

    #[test]
    fn let_shadows_parameter() {
        Test::new(
            r#"module Test
            func test(x: Int) {
                let x: Int = 42;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_shadows_let() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int = 1;
                let x: Int = 2;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_shadowing() {
        // Nested blocks use grouping syntax, not code blocks
        // Simplified test with just sequential shadowing
        Test::new(
            r#"module Test
            func test() {
                let x: Int = 1;
                let x: Int = 2;
                let x: Int = 3;
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod parameter_usage {
    use super::*;

    #[test]
    fn use_parameter_in_body() {
        Test::new(
            r#"module Test
            func test(x: Int) {
                x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn use_multiple_parameters() {
        Test::new(
            r#"module Test
            func test(x: Int, y: Int) {
                x;
                y
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn use_labeled_parameter() {
        Test::new(
            r#"module Test
            func test(label name: Int) {
                name
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod expression_statements {
    use super::*;

    #[test]
    fn expression_statement_literal() {
        Test::new(
            r#"module Test
            func test() {
                42;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn expression_statement_path() {
        Test::new(
            r#"module Test
            func test() {
                foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_expression_statements() {
        Test::new(
            r#"module Test
            func test() {
                42;
                "hello";
                true;
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod complex_expressions {
    use super::*;

    #[test]
    fn nested_arrays() {
        Test::new(
            r#"module Test
            func test() {
                [[1, 2], [3, 4]];
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_tuples() {
        Test::new(
            r#"module Test
            func test() {
                ((1, 2), (3, 4));
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_of_tuples() {
        Test::new(
            r#"module Test
            func test() {
                [(1, 2), (3, 4)];
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_of_arrays() {
        Test::new(
            r#"module Test
            func test() {
                ([1, 2], [3, 4]);
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested_grouping() {
        Test::new(
            r#"module Test
            func test() {
                ((((42))));
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn mixed_literals() {
        Test::new(
            r#"module Test
            func test() {
                (42, 3.14, "hello", true, false);
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn empty_array() {
        Test::new(
            r#"module Test
            func test() {
                [];
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn empty_tuple_is_unit() {
        Test::new(
            r#"module Test
            func test() {
                ();
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn single_element_tuple() {
        Test::new(
            r#"module Test
            func test() {
                (42,);
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn paths_in_complex_expressions() {
        Test::new(
            r#"module Test
            func test() {
                (foo, bar.baz, [qux]);
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_nested_array_type() {
        Test::new(
            r#"module Test
            func test() {
                let x: [[Int]] = [[1, 2], [3, 4]];
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_function_type() {
        Test::new(
            r#"module Test
            func test() {
                let f: (Int) -> Int = foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_tuple_type() {
        Test::new(
            r#"module Test
            func test() {
                let pair: (Int, Int) = (1, 2);
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn hex_integer_literal() {
        Test::new(
            r#"module Test
            func test() {
                0xFF;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn binary_integer_literal() {
        Test::new(
            r#"module Test
            func test() {
                0b1010;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn octal_integer_literal() {
        Test::new(
            r#"module Test
            func test() {
                0o777;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn float_with_exponent() {
        Test::new(
            r#"module Test
            func test() {
                1.5e10;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn underscore_in_numbers() {
        Test::new(
            r#"module Test
            func test() {
                1_000_000
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn underscore_in_hex() {
        Test::new(
            r#"module Test
            func test() {
                0xFF_FF
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_with_escapes() {
        Test::new(
            r#"module Test
            func test() {
                "hello\nworld";
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn path_with_many_segments() {
        Test::new(
            r#"module Test
            func test() {
                a.b.c.d.e.f.g.h.i.j;
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod edge_cases {
    use super::*;

    #[test]
    fn trailing_comma_in_array() {
        Test::new(
            r#"module Test
            func test() {
                [1, 2, 3,]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn trailing_comma_in_tuple() {
        Test::new(
            r#"module Test
            func test() {
                (1, 2, 3,)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn negative_number_literal() {
        Test::new(
            r#"module Test
            func test() {
                -42
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn negative_float_literal() {
        Test::new(
            r#"module Test
            func test() {
                -3.14
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn double_negation() {
        Test::new(
            r#"module Test
            func test() {
                --42
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn not_boolean() {
        Test::new(
            r#"module Test
            func test() {
                !true
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn double_not() {
        Test::new(
            r#"module Test
            func test() {
                !!false
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn negative_path() {
        Test::new(
            r#"module Test
            func test() {
                -x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn not_path() {
        Test::new(
            r#"module Test
            func test() {
                !x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn negative_in_array() {
        Test::new(
            r#"module Test
            func test() {
                [-1, -2, -3]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn negative_in_tuple() {
        Test::new(
            r#"module Test
            func test() {
                (-1, -2)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn negative_grouped() {
        Test::new(
            r#"module Test
            func test() {
                -(42)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn not_grouped() {
        Test::new(
            r#"module Test
            func test() {
                !(true)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn very_large_integer() {
        Test::new(
            r#"module Test
            func test() {
                999_999_999_999_999_999
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unicode_in_string() {
        Test::new(
            r#"module Test
            func test() {
                "Hello 世界 🌍"
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn empty_string() {
        Test::new(
            r#"module Test
            func test() {
                ""
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn whitespace_in_expressions() {
        Test::new(
            r#"module Test
            func test() {
                [   1   ,   2   ,   3   ]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn newlines_in_array() {
        Test::new(
            r#"module Test
            func test() {
                [
                    1,
                    2,
                    3
                ]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_never_type() {
        Test::new(
            r#"module Test
            func test() {
                let x: ! = foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_unit_type() {
        Test::new(
            r#"module Test
            func test() {
                let x: () = ();
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn path_starting_with_underscore() {
        Test::new(
            r#"module Test
            func test() {
                _private.field
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unicode_identifier_path() {
        Test::new(
            r#"module Test
            func test() {
                café.naïve.日本語
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested_array_type() {
        Test::new(
            r#"module Test
            func test() {
                let x: [[[Int]]] = [[[1]]];
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_type_returning_function() {
        Test::new(
            r#"module Test
            func test() {
                let f: (Int) -> (Int) -> Int = foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_of_function_type() {
        Test::new(
            r#"module Test
            func test() {
                let fs: [(Int) -> Int] = [];
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_with_single_function_type() {
        Test::new(
            r#"module Test
            func test() {
                let t: ((Int) -> Int,) = (foo,);
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn var_immediately_reassigned() {
        Test::new(
            r#"module Test
            func test() {
                var x: Int = 1;
                var x: Int = 2;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn many_shadowed_variables() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int = 1;
                let x: Int = 2;
                let x: Int = 3;
                let x: Int = 4;
                let x: Int = 5;
                let x: Int = 6;
                let x: Int = 7;
                let x: Int = 8;
                let x: Int = 9;
                let x: Int = 10;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn parameter_with_same_name_as_type() {
        Test::new(
            r#"module Test
            func test(Int: Int) {
                Int
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn zero_literal() {
        Test::new(
            r#"module Test
            func test() {
                0
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn hex_zero() {
        Test::new(
            r#"module Test
            func test() {
                0x0
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn binary_zero() {
        Test::new(
            r#"module Test
            func test() {
                0b0
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn octal_zero() {
        Test::new(
            r#"module Test
            func test() {
                0o0
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn float_zero() {
        Test::new(
            r#"module Test
            func test() {
                0.0
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn scientific_notation_negative_exponent() {
        Test::new(
            r#"module Test
            func test() {
                1.0e-10
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn scientific_notation_positive_exponent() {
        Test::new(
            r#"module Test
            func test() {
                1.0e+10
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn uppercase_scientific_notation() {
        Test::new(
            r#"module Test
            func test() {
                1.0E10
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn max_hex_value() {
        Test::new(
            r#"module Test
            func test() {
                0xFFFFFFFFFFFFFFFF
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn statement_after_trailing_expression() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int = 1;
                42
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn many_statements_then_expression() {
        Test::new(
            r#"module Test
            func test() {
                let a: Int = 1;
                let b: Int = 2;
                let c: Int = 3;
                let d: Int = 4;
                let e: Int = 5;
                a
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn complex_generic_type() {
        Test::new(
            r#"module Test
            func test() {
                let x: Map[String, List[Option[Int]]] = foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn qualified_type_path() {
        Test::new(
            r#"module Test
            func test() {
                let x: A.B.C.D = foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn comment_in_function_body() {
        Test::new(
            r#"module Test
            func test() {
                // This is a comment
                42
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn block_comment_in_expression() {
        Test::new(
            r#"module Test
            func test() {
                [1, /* comment */ 2, 3]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_block_comments() {
        Test::new(
            r#"module Test
            func test() {
                /* outer /* inner */ still outer */
                42
            }
        "#,
        )
        .expect(Compiles);
    }

    // Stress tests for complex nesting
    #[test]
    fn crazy_nesting() {
        Test::new(
            r#"module Test
            func test() {
                [([(-1,)],)]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_of_negatives() {
        Test::new(
            r#"module Test
            func test() {
                [-1, -2, -3, -4, -5, -6, -7, -8, -9, -10]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_of_negatives() {
        Test::new(
            r#"module Test
            func test() {
                (-1, -2, -3, -4, -5)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn mixed_unary_operators() {
        Test::new(
            r#"module Test
            func test() {
                (-x, !y, --z, !!w)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unary_on_complex_path() {
        Test::new(
            r#"module Test
            func test() {
                -a.b.c.d
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn not_on_complex_path() {
        Test::new(
            r#"module Test
            func test() {
                !a.b.c.d
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_negative_init() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int = -42;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn let_with_not_init() {
        Test::new(
            r#"module Test
            func test() {
                let x: Bool = !false;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested_unary() {
        Test::new(
            r#"module Test
            func test() {
                -----42
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested_not() {
        Test::new(
            r#"module Test
            func test() {
                !!!!!true
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn alternating_unary() {
        Test::new(
            r#"module Test
            func test() {
                -!-!x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unary_in_nested_expression() {
        Test::new(
            r#"module Test
            func test() {
                ((-1, -2), (-3, -4))
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unary_with_all_literal_types() {
        Test::new(
            r#"module Test
            func test() {
                -42;
                -3.14;
                !true;
                !false;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_variables_with_unary() {
        Test::new(
            r#"module Test
            func test() {
                let a: Int = -1;
                let b: Int = -2;
                let c: Bool = !true;
                let d: Bool = !false;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_with_all_negatives() {
        Test::new(
            r#"module Test
            func test() {
                [-0x1, -0b1, -0o1, -1]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn negative_float_scientific() {
        Test::new(
            r#"module Test
            func test() {
                -1.0e10
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn negative_float_negative_exponent() {
        Test::new(
            r#"module Test
            func test() {
                -1.0e-10
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unary_with_grouping() {
        Test::new(
            r#"module Test
            func test() {
                (-(-(-(42))))
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn bool_expressions() {
        Test::new(
            r#"module Test
            func test() {
                (!(!(!true)));
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn parameter_with_negation() {
        Test::new(
            r#"module Test
            func test(x: Int) {
                -x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn parameter_with_not() {
        Test::new(
            r#"module Test
            func test(b: Bool) {
                !b
            }
        "#,
        )
        .expect(Compiles);
    }

    // Test null literal
    #[test]
    fn null_literal() {
        Test::new(
            r#"module Test
            func test() {
                null
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn null_in_array() {
        Test::new(
            r#"module Test
            func test() {
                [null, null, null]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn null_in_tuple() {
        Test::new(
            r#"module Test
            func test() {
                (null, 42, null)
            }
        "#,
        )
        .expect(Compiles);
    }

    // Test expression as variable initializer
    #[test]
    fn unary_expression_as_initializer() {
        Test::new(
            r#"module Test
            func test() {
                let x: Int = -foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn complex_initializer() {
        Test::new(
            r#"module Test
            func test() {
                let x: (Int, Int) = (-1, -2);
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_initializer() {
        Test::new(
            r#"module Test
            func test() {
                let x: [Int] = [-1, -2, -3];
            }
        "#,
        )
        .expect(Compiles);
    }

    // Super stress tests
    #[test]
    fn very_deeply_nested_arrays() {
        Test::new(
            r#"module Test
            func test() {
                [[[[[[1]]]]]]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn very_deeply_nested_tuples() {
        Test::new(
            r#"module Test
            func test() {
                ((((((1,),),),),),)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn mixed_nested_containers() {
        Test::new(
            r#"module Test
            func test() {
                [([[(1,)]],)]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn many_unary_operators() {
        Test::new(
            r#"module Test
            func test() {
                ----------10
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn many_bang_operators() {
        Test::new(
            r#"module Test
            func test() {
                !!!!!!!!!!true
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn alternating_many_unary() {
        Test::new(
            r#"module Test
            func test() {
                -!-!-!-!-!x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unary_inside_deeply_nested() {
        Test::new(
            r#"module Test
            func test() {
                (((((-42)))))
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn array_of_nulls_and_negatives() {
        Test::new(
            r#"module Test
            func test() {
                [null, -1, null, -2, null, -3]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_of_all_types() {
        Test::new(
            r#"module Test
            func test() {
                (42, -42, 3.14, -3.14, "hello", true, false, null, (), x, a.b.c)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn complex_let_with_unary_initializer() {
        Test::new(
            r#"module Test
            func test() {
                let x: (Int, Bool, Int) = (-1, !true, --2);
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn many_variables_many_types() {
        Test::new(
            r#"module Test
            func test() {
                let a: Int = 1;
                let b: Float = 2.0;
                let c: String = "hello";
                let d: Bool = true;
                let e: [Int] = [1, 2, 3];
                let f: (Int, Int) = (1, 2);
                let g: () = ();
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_many_parameters() {
        Test::new(
            r#"module Test
            func test(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) {
                a;
                b;
                c;
                d;
                e;
                f;
                g;
                h
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn labeled_and_unlabeled_parameters() {
        Test::new(
            r#"module Test
            func test(label1 a: Int, b: Int, label2 c: Int, d: Int) {
                (a, b, c, d)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested_generic_types() {
        Test::new(
            r#"module Test
            func test() {
                let x: Option[Result[List[Map[String, Int]], Error]] = foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_type_with_many_params() {
        Test::new(
            r#"module Test
            func test() {
                let f: (Int, Int, Int, Int) -> (Int, Int) = foo;
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn very_long_path() {
        Test::new(
            r#"module Test
            func test() {
                a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unary_on_long_path() {
        Test::new(
            r#"module Test
            func test() {
                -a.b.c.d.e.f.g.h.i.j
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn not_on_long_path() {
        Test::new(
            r#"module Test
            func test() {
                !a.b.c.d.e.f.g.h.i.j
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn empty_function_body_with_unit_return() {
        Test::new(
            r#"module Test
            func test() { () }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn expression_followed_by_declaration() {
        Test::new(
            r#"module Test
            func test() {
                42;
                let x: Int = 1;
                x
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn interspersed_statements_and_expressions() {
        Test::new(
            r#"module Test
            func test() {
                let a: Int = 1;
                42;
                let b: Int = 2;
                "hello";
                let c: Int = 3;
                c
            }
        "#,
        )
        .expect(Compiles);
    }
}
