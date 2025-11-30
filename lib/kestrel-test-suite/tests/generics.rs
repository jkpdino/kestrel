//! Tests for generics implementation
//!
//! This file tests generic type declarations, type parameters,
//! where clauses, and related validation.

use kestrel_test_suite::*;

mod basic_parsing {
    use super::*;

    #[test]
    fn generic_struct_single_param() {
        Test::new("module Test\nstruct Box[T] {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Box")
                    .is(SymbolKind::Struct)
                    .has(Behavior::IsGeneric(true))
                    .has(Behavior::TypeParamCount(1)),
            );
    }

    #[test]
    fn generic_struct_multiple_params() {
        Test::new("module Test\nstruct Pair[A, B] {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Pair")
                    .is(SymbolKind::Struct)
                    .has(Behavior::TypeParamCount(2)),
            );
    }

    #[test]
    fn generic_struct_three_params() {
        Test::new("module Test\nstruct Triple[X, Y, Z] {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Triple")
                    .is(SymbolKind::Struct)
                    .has(Behavior::TypeParamCount(3)),
            );
    }

    #[test]
    fn non_generic_struct() {
        Test::new("module Test\nstruct Plain {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Plain")
                    .is(SymbolKind::Struct)
                    .has(Behavior::IsGeneric(false))
                    .has(Behavior::TypeParamCount(0)),
            );
    }

    #[test]
    fn generic_protocol() {
        Test::new(
            r#"module Test
            protocol Container[T] {
                func get() -> ()
            }
        "#,
        )
        .expect(Compiles)
        .expect(
            Symbol::new("Container")
                .is(SymbolKind::Protocol)
                .has(Behavior::IsGeneric(true))
                .has(Behavior::TypeParamCount(1)),
        );
    }

    #[test]
    fn generic_function() {
        Test::new("module Test\nfunc identity[T](value: T) -> T { }")
            .expect(Compiles)
            .expect(
                Symbol::new("identity")
                    .is(SymbolKind::Function)
                    .has(Behavior::IsGeneric(true))
                    .has(Behavior::TypeParamCount(1)),
            );
    }

    #[test]
    fn generic_type_alias() {
        Test::new("module Test\ntype List[T] = T;")
            .expect(Compiles)
            .expect(
                Symbol::new("List")
                    .is(SymbolKind::TypeAlias)
                    .has(Behavior::IsGeneric(true))
                    .has(Behavior::TypeParamCount(1)),
            );
    }
}

mod defaults {
    use super::*;

    #[test]
    fn type_param_with_default() {
        Test::new("module Test\nstruct Map[K, V = String] {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Map")
                    .is(SymbolKind::Struct)
                    .has(Behavior::TypeParamCount(2)),
            );
    }

    #[test]
    fn all_params_with_defaults() {
        Test::new("module Test\nstruct Wrapper[T = Int] {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Wrapper")
                    .is(SymbolKind::Struct)
                    .has(Behavior::TypeParamCount(1)),
            );
    }

    #[test]
    fn use_default_type_argument() {
        // Map[K, V = String] can be used as Map[Int] with V defaulting to String
        Test::new(
            r#"module Test
            struct Map[K, V = String] { }
            type IntMap = Map[Int];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn override_default_type_argument() {
        // Can still provide explicit value for defaulted parameter
        Test::new(
            r#"module Test
            struct Map[K, V = String] { }
            type IntToInt = Map[Int, Int];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_defaults() {
        Test::new(
            r#"module Test
            struct Config[A, B = Int, C = String] { }
            type SimpleConfig = Config[Bool];
            type CustomConfig = Config[Bool, Float];
        "#,
        )
        .expect(Compiles);
    }
}

mod validation {
    use super::*;

    #[test]
    fn duplicate_type_param_error() {
        Test::new("module Test\nstruct Bad[T, T] {}")
            .expect(HasError("duplicate type parameter 'T'"));
    }

    #[test]
    fn duplicate_type_param_in_function() {
        Test::new("module Test\nfunc bad[A, A]() { }")
            .expect(HasError("duplicate type parameter 'A'"));
    }

    #[test]
    fn default_ordering_error() {
        Test::new("module Test\nstruct Bad[T = Int, U] {}")
            .expect(HasError("with default must come after"));
    }

    #[test]
    fn default_ordering_valid() {
        // This should compile - defaults come after non-defaults
        Test::new("module Test\nstruct Good[T, U = Int] {}")
            .expect(Compiles)
            .expect(Symbol::new("Good").has(Behavior::TypeParamCount(2)));
    }
}

mod where_clause {
    use super::*;

    #[test]
    fn simple_where_clause() {
        Test::new(
            r#"module Test
            protocol Equatable { }
            struct Set[T] where T: Equatable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Set").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn where_clause_multiple_bounds() {
        Test::new(
            r#"module Test
            protocol Equatable { }
            protocol Hashable { }
            struct HashSet[T] where T: Equatable and Hashable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("HashSet").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn where_clause_on_function() {
        Test::new(
            r#"module Test
            protocol Comparable { }
            func sort[T](items: T) where T: Comparable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("sort").has(Behavior::IsGeneric(true)));
    }

    #[test]
    fn where_clause_unresolved_bound() {
        // Test that where clause with non-existent protocol produces error
        Test::new(
            r#"module Test
            struct Set[T] where T: NonExistent { }
        "#,
        )
        .expect(HasError("'NonExistent' is not a protocol"));
    }

    #[test]
    fn where_clause_bound_is_struct() {
        // Test that where clause with struct instead of protocol produces error
        Test::new(
            r#"module Test
            struct SomeStruct { }
            struct Container[T] where T: SomeStruct { }
        "#,
        )
        .expect(HasError("'SomeStruct' is not a protocol"));
    }

    #[test]
    fn where_clause_bound_is_type_alias() {
        // Test that where clause with type alias instead of protocol produces error
        Test::new(
            r#"module Test
            type MyAlias = Int;
            struct Container[T] where T: MyAlias { }
        "#,
        )
        .expect(HasError("'MyAlias' is not a protocol"));
    }

    #[test]
    fn where_clause_valid_protocol_bound() {
        // Test that valid protocol bounds work correctly
        Test::new(
            r#"module Test
            protocol Display { }
            protocol Debug { }
            struct Logger[T] where T: Display and Debug { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Logger").has(Behavior::TypeParamCount(1)));
    }
}

mod nested_generics {
    use super::*;

    #[test]
    fn generic_inside_generic() {
        Test::new(
            r#"module Test
            struct Outer[T] {
                struct Inner[U] { }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Outer").has(Behavior::TypeParamCount(1)))
        .expect(Symbol::new("Inner").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn non_generic_inside_generic() {
        Test::new(
            r#"module Test
            struct Container[T] {
                struct Plain { }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Container").has(Behavior::IsGeneric(true)))
        .expect(Symbol::new("Plain").has(Behavior::IsGeneric(false)));
    }
}

mod instantiation {
    use super::*;

    #[test]
    fn generic_field_type() {
        // Test that generic types can be used as field types
        Test::new(
            r#"module Test
            struct Box[T] {
                let value: T
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Box").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn generic_return_type() {
        // Test that generic types can be used as return types
        Test::new(
            r#"module Test
            struct Box[T] { }
            func makeBox[T]() -> Box[T] { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("makeBox").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn generic_parameter_type() {
        // Test that generic types can be used as parameter types
        Test::new(
            r#"module Test
            struct Box[T] { }
            func unbox[T](box: Box[T]) -> T { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("unbox").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn nested_generic_types() {
        // Test nested generic type usage: Box[Box[Int]]
        Test::new(
            r#"module Test
            struct Box[T] { }
            type NestedBox = Box[Box[Int]];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_type_args() {
        // Test types with multiple type arguments
        Test::new(
            r#"module Test
            struct Map[K, V] { }
            type StringToInt = Map[String, Int];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn generic_type_in_protocol() {
        // Test generic types used in protocol method signatures
        Test::new(
            r#"module Test
            struct Box[T] { }
            protocol Container[T] {
                func wrap(value: T) -> Box[T]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_with_generic() {
        // Test generic types inside tuples
        Test::new(
            r#"module Test
            struct Box[T] { }
            func pair[A, B](a: A, b: B) -> (Box[A], Box[B]) { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_type_with_generic() {
        // Test generic types in function type signatures
        Test::new(
            r#"module Test
            struct Box[T] { }
            func transform[T](f: (T) -> Box[T], value: T) -> Box[T] { }
        "#,
        )
        .expect(Compiles);
    }
}
