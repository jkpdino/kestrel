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
