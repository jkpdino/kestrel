use kestrel_test_suite::*;

/// Macro for simple compilation tests that wrap code in a function body
macro_rules! compiles {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            Test::new(concat!("module Test\nfunc test() {\n", $code, "\n}")).expect(Compiles);
        }
    };
}

/// Macro for tests with custom function signatures
macro_rules! compiles_fn {
    ($name:ident, $sig:expr, $body:expr) => {
        #[test]
        fn $name() {
            Test::new(concat!("module Test\nfunc test", $sig, " {\n", $body, "\n}")).expect(Compiles);
        }
    };
}

mod path_expressions {
    use super::*;

    compiles!(path_single_segment, "foo");
    compiles!(path_two_segments, "foo.bar");
    compiles!(path_multiple_segments, "a.b.c.d");
    compiles!(path_in_array, "[foo, bar]");
    compiles!(path_in_tuple, "(foo, bar)");
    compiles!(path_in_grouping, "(foo)");
}

mod variable_declarations {
    use super::*;

    compiles!(let_with_type, "let x: Int = 42;");
    compiles!(var_with_type, "var x: Int = 42;");
    compiles!(let_without_initializer, "let x: Int;");
    compiles!(multiple_declarations, "let x: Int = 1;\nlet y: Int = 2;\nlet z: Int = 3;");
    compiles!(let_with_path_initializer, "let x: Int = foo;");
    compiles!(let_with_complex_type, r#"let x: (Int, String) = (1, "hello");"#);
    compiles!(let_with_array_type, "let x: [Int] = [1, 2, 3];");
}

mod variable_shadowing {
    use super::*;

    compiles_fn!(let_shadows_parameter, "(x: Int)", "let x: Int = 42;");
    compiles!(let_shadows_let, "let x: Int = 1;\nlet x: Int = 2;");
    // Nested blocks use grouping syntax, not code blocks
    // Simplified test with just sequential shadowing
    compiles!(nested_shadowing, "let x: Int = 1;\nlet x: Int = 2;\nlet x: Int = 3;");
}

mod parameter_usage {
    use super::*;

    compiles_fn!(use_parameter_in_body, "(x: Int)", "x");
    compiles_fn!(use_multiple_parameters, "(x: Int, y: Int)", "x;\ny");
    compiles_fn!(use_labeled_parameter, "(label name: Int)", "name");
}

mod expression_statements {
    use super::*;

    compiles!(expression_statement_literal, "42;");
    compiles!(expression_statement_path, "foo;");
    compiles!(multiple_expression_statements, "42;\n\"hello\";\ntrue;");
}

mod complex_expressions {
    use super::*;

    compiles!(nested_arrays, "[[1, 2], [3, 4]];");
    compiles!(nested_tuples, "((1, 2), (3, 4));");
    compiles!(array_of_tuples, "[(1, 2), (3, 4)];");
    compiles!(tuple_of_arrays, "([1, 2], [3, 4]);");
    compiles!(deeply_nested_grouping, "((((42))));");
    compiles!(mixed_literals, r#"(42, 3.14, "hello", true, false);"#);
    compiles!(empty_array, "[];");
    compiles!(empty_tuple_is_unit, "();");
    compiles!(single_element_tuple, "(42,);");
    compiles!(paths_in_complex_expressions, "(foo, bar.baz, [qux]);");
    compiles!(let_with_nested_array_type, "let x: [[Int]] = [[1, 2], [3, 4]];");
    compiles!(let_with_function_type, "let f: (Int) -> Int = foo;");
    compiles!(let_with_tuple_type, "let pair: (Int, Int) = (1, 2);");
    compiles!(hex_integer_literal, "0xFF;");
    compiles!(binary_integer_literal, "0b1010;");
    compiles!(octal_integer_literal, "0o777;");
    compiles!(float_with_exponent, "1.5e10;");
    compiles!(underscore_in_numbers, "1_000_000");
    compiles!(underscore_in_hex, "0xFF_FF");
    compiles!(string_with_escapes, r#""hello\nworld";"#);
    compiles!(path_with_many_segments, "a.b.c.d.e.f.g.h.i.j;");
}

mod edge_cases {
    use super::*;

    compiles!(trailing_comma_in_array, "[1, 2, 3,]");
    compiles!(trailing_comma_in_tuple, "(1, 2, 3,)");
    compiles!(negative_number_literal, "-42");
    compiles!(negative_float_literal, "-3.14");
    compiles!(double_negation, "--42");
    compiles!(not_boolean, "!true");
    compiles!(double_not, "!!false");
    compiles!(negative_path, "-x");
    compiles!(not_path, "!x");
    compiles!(negative_in_array, "[-1, -2, -3]");
    compiles!(negative_in_tuple, "(-1, -2)");
    compiles!(negative_grouped, "-(42)");
    compiles!(not_grouped, "!(true)");
    compiles!(very_large_integer, "999_999_999_999_999_999");
    compiles!(unicode_in_string, r#""Hello 世界 🌍""#);
    compiles!(empty_string, r#""""#);
    compiles!(whitespace_in_expressions, "[   1   ,   2   ,   3   ]");
    compiles!(newlines_in_array, "[\n    1,\n    2,\n    3\n]");
    compiles!(let_with_never_type, "let x: ! = foo;");
    compiles!(let_with_unit_type, "let x: () = ();");
    compiles!(path_starting_with_underscore, "_private.field");
    compiles!(unicode_identifier_path, "café.naïve.日本語");
    compiles!(deeply_nested_array_type, "let x: [[[Int]]] = [[[1]]];");
    compiles!(function_type_returning_function, "let f: (Int) -> (Int) -> Int = foo;");
    compiles!(array_of_function_type, "let fs: [(Int) -> Int] = [];");
    compiles!(tuple_with_single_function_type, "let t: ((Int) -> Int,) = (foo,);");
    compiles!(var_immediately_reassigned, "var x: Int = 1;\nvar x: Int = 2;");
    compiles!(many_shadowed_variables,
        "let x: Int = 1;\nlet x: Int = 2;\nlet x: Int = 3;\nlet x: Int = 4;\nlet x: Int = 5;\n\
         let x: Int = 6;\nlet x: Int = 7;\nlet x: Int = 8;\nlet x: Int = 9;\nlet x: Int = 10;");
    compiles_fn!(parameter_with_same_name_as_type, "(Int: Int)", "Int");
    compiles!(zero_literal, "0");
    compiles!(hex_zero, "0x0");
    compiles!(binary_zero, "0b0");
    compiles!(octal_zero, "0o0");
    compiles!(float_zero, "0.0");
    compiles!(scientific_notation_negative_exponent, "1.0e-10");
    compiles!(scientific_notation_positive_exponent, "1.0e+10");
    compiles!(uppercase_scientific_notation, "1.0E10");
    compiles!(max_hex_value, "0xFFFFFFFFFFFFFFFF");
    compiles!(statement_after_trailing_expression, "let x: Int = 1;\n42");
    compiles!(many_statements_then_expression,
        "let a: Int = 1;\nlet b: Int = 2;\nlet c: Int = 3;\nlet d: Int = 4;\nlet e: Int = 5;\na");
    compiles!(complex_generic_type, "let x: Map[String, List[Option[Int]]] = foo;");
    compiles!(qualified_type_path, "let x: A.B.C.D = foo;");
    compiles!(comment_in_function_body, "// This is a comment\n42");
    compiles!(block_comment_in_expression, "[1, /* comment */ 2, 3]");
    compiles!(nested_block_comments, "/* outer /* inner */ still outer */\n42");
    compiles!(crazy_nesting, "[([(-1,)],)]");
    compiles!(array_of_negatives, "[-1, -2, -3, -4, -5, -6, -7, -8, -9, -10]");
    compiles!(tuple_of_negatives, "(-1, -2, -3, -4, -5)");
    compiles!(mixed_unary_operators, "(-x, !y, --z, !!w)");
    compiles!(unary_on_complex_path, "-a.b.c.d");
    compiles!(not_on_complex_path, "!a.b.c.d");
    compiles!(let_with_negative_init, "let x: Int = -42;");
    compiles!(let_with_not_init, "let x: Bool = !false;");
    compiles!(deeply_nested_unary, "-----42");
    compiles!(deeply_nested_not, "!!!!!true");
    compiles!(alternating_unary, "-!-!x");
    compiles!(unary_in_nested_expression, "((-1, -2), (-3, -4))");
    compiles!(unary_with_all_literal_types, "-42;\n-3.14;\n!true;\n!false;");
    compiles!(multiple_variables_with_unary,
        "let a: Int = -1;\nlet b: Int = -2;\nlet c: Bool = !true;\nlet d: Bool = !false;");
    compiles!(array_with_all_negatives, "[-0x1, -0b1, -0o1, -1]");
    compiles!(negative_float_scientific, "-1.0e10");
    compiles!(negative_float_negative_exponent, "-1.0e-10");
    compiles!(unary_with_grouping, "(-(-(-(42))))");
    compiles!(bool_expressions, "(!(!(!true)));");
    compiles_fn!(parameter_with_negation, "(x: Int)", "-x");
    compiles_fn!(parameter_with_not, "(b: Bool)", "!b");
    compiles!(null_literal, "null");
    compiles!(null_in_array, "[null, null, null]");
    compiles!(null_in_tuple, "(null, 42, null)");
    compiles!(unary_expression_as_initializer, "let x: Int = -foo;");
    compiles!(complex_initializer, "let x: (Int, Int) = (-1, -2);");
    compiles!(array_initializer, "let x: [Int] = [-1, -2, -3];");
    compiles!(very_deeply_nested_arrays, "[[[[[[1]]]]]]");
    compiles!(very_deeply_nested_tuples, "((((((1,),),),),),)");
    compiles!(mixed_nested_containers, "[([[(1,)]],)]");
    compiles!(many_unary_operators, "----------10");
    compiles!(many_bang_operators, "!!!!!!!!!!true");
    compiles!(alternating_many_unary, "-!-!-!-!-!x");
    compiles!(unary_inside_deeply_nested, "(((((-42)))))");
    compiles!(array_of_nulls_and_negatives, "[null, -1, null, -2, null, -3]");
    compiles!(tuple_of_all_types, r#"(42, -42, 3.14, -3.14, "hello", true, false, null, (), x, a.b.c)"#);
    compiles!(complex_let_with_unary_initializer, "let x: (Int, Bool, Int) = (-1, !true, --2);");
    compiles!(many_variables_many_types,
        "let a: Int = 1;\nlet b: Float = 2.0;\nlet c: String = \"hello\";\nlet d: Bool = true;\n\
         let e: [Int] = [1, 2, 3];\nlet f: (Int, Int) = (1, 2);\nlet g: () = ();");
    compiles_fn!(function_with_many_parameters,
        "(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int)",
        "a;\nb;\nc;\nd;\ne;\nf;\ng;\nh");
    compiles_fn!(labeled_and_unlabeled_parameters,
        "(label1 a: Int, b: Int, label2 c: Int, d: Int)",
        "(a, b, c, d)");
    compiles!(deeply_nested_generic_types, "let x: Option[Result[List[Map[String, Int]], Error]] = foo;");
    compiles!(function_type_with_many_params, "let f: (Int, Int, Int, Int) -> (Int, Int) = foo;");
    compiles!(very_long_path, "a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t");
    compiles!(unary_on_long_path, "-a.b.c.d.e.f.g.h.i.j");
    compiles!(not_on_long_path, "!a.b.c.d.e.f.g.h.i.j");
    compiles!(empty_function_body_with_unit_return, "()");
    compiles!(expression_followed_by_declaration, "42;\nlet x: Int = 1;\nx");
    compiles!(interspersed_statements_and_expressions,
        "let a: Int = 1;\n42;\nlet b: Int = 2;\n\"hello\";\nlet c: Int = 3;\nc");
}
