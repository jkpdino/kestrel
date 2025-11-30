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

mod arity_errors {
    use super::*;

    #[test]
    fn too_few_type_arguments() {
        // Map[K, V] requires 2 type arguments, only 1 provided
        Test::new(
            r#"module Test
            struct Map[K, V] { }
            type Bad = Map[Int];
        "#,
        )
        .expect(HasError("type argument"));
    }

    #[test]
    fn too_many_type_arguments() {
        // Box[T] takes only 1 type argument, 2 provided
        Test::new(
            r#"module Test
            struct Box[T] { }
            type Bad = Box[Int, String];
        "#,
        )
        .expect(HasError("type argument"));
    }

    #[test]
    fn zero_type_arguments_when_required() {
        // Using a generic type without [] syntax is a raw type reference, not an instantiation.
        // This is allowed (e.g., for passing to higher-order generics or future type inference).
        // Using Box[] (empty brackets) would be different and could trigger arity checking.
        Test::new(
            r#"module Test
            struct Box[T] { }
            type Alias = Box;
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn correct_arity_with_defaults() {
        // Map[K, V = String] allows 1 or 2 type arguments
        Test::new(
            r#"module Test
            struct Map[K, V = String] { }
            type IntMap = Map[Int];
            type IntToInt = Map[Int, Int];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn too_few_even_with_defaults() {
        // Triple[A, B, C = Int] requires at least 2 type arguments
        Test::new(
            r#"module Test
            struct Triple[A, B, C = Int] { }
            type Bad = Triple[Int];
        "#,
        )
        .expect(HasError("type argument"));
    }
}

mod non_generic_errors {
    use super::*;

    #[test]
    fn type_args_on_non_generic_struct() {
        Test::new(
            r#"module Test
            struct Plain { }
            type Bad = Plain[Int];
        "#,
        )
        .expect(HasError("does not accept type arguments"));
    }

    #[test]
    fn type_args_on_non_generic_type_alias() {
        Test::new(
            r#"module Test
            type Simple = Int;
            type Bad = Simple[String];
        "#,
        )
        .expect(HasError("does not accept type arguments"));
    }

    #[test]
    fn type_args_on_primitive() {
        Test::new(
            r#"module Test
            type Bad = Int[String];
        "#,
        )
        .expect(HasError("does not accept type arguments"));
    }
}

mod undeclared_type_params {
    use super::*;

    #[test]
    fn undeclared_in_where_clause() {
        // U is not declared in the type parameter list
        Test::new(
            r#"module Test
            protocol Equatable { }
            struct Set[T] where U: Equatable { }
        "#,
        )
        .expect(HasError("undeclared type parameter"));
    }

    #[test]
    fn undeclared_in_function_where_clause() {
        Test::new(
            r#"module Test
            protocol Comparable { }
            func sort[T](items: T) where U: Comparable { }
        "#,
        )
        .expect(HasError("undeclared type parameter"));
    }

    #[test]
    fn typo_in_where_clause() {
        // Tx is a typo for T
        Test::new(
            r#"module Test
            protocol Display { }
            struct Printer[T] where Tx: Display { }
        "#,
        )
        .expect(HasError("undeclared type parameter"));
    }
}

mod type_alias_resolution {
    use super::*;

    #[test]
    fn identity_type_alias() {
        // type Identity[T] = T should be a valid generic type alias
        Test::new(
            r#"module Test
            type Identity[T] = T;
        "#,
        )
        .expect(Compiles)
        .expect(
            Symbol::new("Identity")
                .is(SymbolKind::TypeAlias)
                .has(Behavior::TypeParamCount(1)),
        );
    }

    #[test]
    fn identity_type_alias_instantiated() {
        // Using Identity[Int] should work
        Test::new(
            r#"module Test
            type Identity[T] = T;
            type IntAlias = Identity[Int];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn pair_type_alias() {
        // type Pair[T] = (T, T) - using type param multiple times
        Test::new(
            r#"module Test
            type Pair[T] = (T, T);
            type IntPair = Pair[Int];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_type_param_in_alias() {
        // Type param used as argument to another generic
        Test::new(
            r#"module Test
            struct Box[T] { }
            type Boxed[T] = Box[T];
            type BoxedInt = Boxed[Int];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn type_alias_with_function_type() {
        // type Transformer[A, B] = (A) -> B
        Test::new(
            r#"module Test
            type Transformer[A, B] = (A) -> B;
            type IntToString = Transformer[Int, String];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn generic_alias_chaining() {
        // Chain of generic type aliases
        Test::new(
            r#"module Test
            struct Box[T] { }
            type Boxed[T] = Box[T];
            type DoubleBoxed[T] = Boxed[Boxed[T]];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn type_param_in_nested_tuple() {
        Test::new(
            r#"module Test
            type Nested[T] = ((T, Int), (String, T));
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Nested").has(Behavior::TypeParamCount(1)));
    }
}

mod multiple_constraints {
    use super::*;

    #[test]
    fn two_params_with_separate_bounds() {
        Test::new(
            r#"module Test
            protocol Equatable { }
            protocol Hashable { }
            struct BiMap[K, V] where K: Equatable, V: Hashable { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn three_params_with_mixed_bounds() {
        Test::new(
            r#"module Test
            protocol A { }
            protocol B { }
            protocol C { }
            struct Complex[X, Y, Z] where X: A, Y: B and C, Z: A { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn same_param_multiple_separate_constraints() {
        // T has two separate constraint clauses (if syntax allows)
        Test::new(
            r#"module Test
            protocol Display { }
            protocol Debug { }
            struct Logger[T] where T: Display, T: Debug { }
        "#,
        )
        .expect(Compiles);
    }
}

mod edge_cases {
    use super::*;

    #[test]
    fn single_letter_type_params() {
        Test::new("module Test\nstruct A[B] {}")
            .expect(Compiles)
            .expect(Symbol::new("A").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn long_type_param_names() {
        Test::new("module Test\nstruct Container[ElementType, KeyType, ValueType] {}")
            .expect(Compiles)
            .expect(Symbol::new("Container").has(Behavior::TypeParamCount(3)));
    }

    #[test]
    fn many_type_params() {
        Test::new("module Test\nstruct Many[A, B, C, D, E, F] {}")
            .expect(Compiles)
            .expect(Symbol::new("Many").has(Behavior::TypeParamCount(6)));
    }

    #[test]
    fn type_param_same_name_as_struct() {
        // Type parameter named same as the struct itself
        Test::new("module Test\nstruct Box[Box] {}")
            .expect(Compiles)
            .expect(Symbol::new("Box").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn self_referential_generic() {
        // A generic type that refers to itself with its own type param
        Test::new(
            r#"module Test
            struct Node[T] {
                let value: T
                let next: Node[T]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn mutually_referential_generics() {
        Test::new(
            r#"module Test
            struct Tree[T] {
                let value: T
                let children: Forest[T]
            }
            struct Forest[T] {
                let trees: Tree[T]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn type_param_shadowing_in_nested() {
        // Inner struct has its own T that shadows outer T
        Test::new(
            r#"module Test
            struct Outer[T] {
                struct Inner[T] {
                    let value: T
                }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Outer").has(Behavior::TypeParamCount(1)))
        .expect(Symbol::new("Inner").has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn generic_protocol_method_using_struct_type_param() {
        Test::new(
            r#"module Test
            struct Box[T] { }
            protocol Factory[T] {
                func create() -> Box[T]
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn where_clause_with_generic_bound() {
        // Bound itself is a generic type: T: Comparable[T]
        Test::new(
            r#"module Test
            protocol Comparable[U] { }
            struct Set[T] where T: Comparable[T] { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested_generics() {
        Test::new(
            r#"module Test
            struct Box[T] { }
            type Deep = Box[Box[Box[Box[Int]]]];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn generic_in_optional_like_pattern() {
        Test::new(
            r#"module Test
            struct Option[T] {
                let value: T
            }
            type OptionalInt = Option[Int];
            type OptionalOptional = Option[Option[String]];
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn generic_alias_preserves_param_count() {
        Test::new(
            r#"module Test
            type Pair[A, B] = (A, B);
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Pair").has(Behavior::TypeParamCount(2)));
    }
}
