use kestrel_test_suite::*;

mod basic {
    use super::*;

    #[test]
    fn simple_type_alias() {
        Test::new("module Test\ntype Simple = Int;")
            .expect(Compiles)
            .expect(Symbol::new("Simple").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn public_type_alias() {
        Test::new("module Test\npublic type PublicAlias = String;")
            .expect(Compiles)
            .expect(
                Symbol::new("PublicAlias")
                    .is(SymbolKind::TypeAlias)
                    .has(Behavior::Visibility(Visibility::Public)),
            );
    }

    #[test]
    fn private_type_alias() {
        Test::new("module Test\nprivate type PrivateAlias = Bool;")
            .expect(Compiles)
            .expect(
                Symbol::new("PrivateAlias")
                    .is(SymbolKind::TypeAlias)
                    .has(Behavior::Visibility(Visibility::Private)),
            );
    }

    #[test]
    fn internal_type_alias() {
        Test::new("module Test\ninternal type InternalAlias = Float;")
            .expect(Compiles)
            .expect(
                Symbol::new("InternalAlias")
                    .is(SymbolKind::TypeAlias)
                    .has(Behavior::Visibility(Visibility::Internal)),
            );
    }

    #[test]
    fn fileprivate_type_alias() {
        Test::new("module Test\nfileprivate type FilePrivateAlias = Int;")
            .expect(Compiles)
            .expect(
                Symbol::new("FilePrivateAlias")
                    .is(SymbolKind::TypeAlias)
                    .has(Behavior::Visibility(Visibility::Fileprivate)),
            );
    }

    #[test]
    fn multiple_type_aliases() {
        Test::new(
            r#"module Test
            type Result = Int;
            type Maybe = String;
            type List = Bool;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Result").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Maybe").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("List").is(SymbolKind::TypeAlias));
    }
}

mod naming {
    use super::*;

    #[test]
    fn camel_case_alias() {
        Test::new("module Test\ntype CamelCaseAlias = Int;")
            .expect(Compiles)
            .expect(Symbol::new("CamelCaseAlias").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn snake_case_alias() {
        Test::new("module Test\ntype snake_case_alias = String;")
            .expect(Compiles)
            .expect(Symbol::new("snake_case_alias").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn screaming_snake_alias() {
        Test::new("module Test\ntype SCREAMING_SNAKE = Bool;")
            .expect(Compiles)
            .expect(Symbol::new("SCREAMING_SNAKE").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn alias_with_numbers() {
        Test::new("module Test\ntype MixedCase123 = Float;")
            .expect(Compiles)
            .expect(Symbol::new("MixedCase123").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn single_character_alias() {
        Test::new("module Test\ntype A = Int;\ntype B = String;")
            .expect(Compiles)
            .expect(Symbol::new("A").is(SymbolKind::TypeAlias))
            .expect(Symbol::new("B").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn alias_with_underscores() {
        Test::new(
            r#"module Test
            type _leading = Int;
            type trailing_ = String;
            type _both_ = Bool;
            type mid_dle = Float;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("_leading").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("trailing_").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("_both_").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("mid_dle").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn alias_with_common_suffixes() {
        Test::new(
            r#"module Test
            type Type1 = Int;
            type Alias0 = String;
            type Map2D = Bool;
            type Vector3D = Float;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Type1").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Alias0").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Map2D").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Vector3D").is(SymbolKind::TypeAlias));
    }
}

mod realistic {
    use super::*;

    #[test]
    fn domain_type_aliases() {
        Test::new(
            r#"module Application.Types
            public type UserID = String;
            public type Email = String;
            public type PhoneNumber = String;
            public type Timestamp = Int;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("UserID").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Email").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("PhoneNumber").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Timestamp").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn collection_aliases() {
        // Define the types that we alias to (public to match alias visibility)
        Test::new(
            r#"module Test
            public struct Array {}
            public struct Dictionary {}
            struct Set {}
            public type UserList = Array;
            public type UserMap = Dictionary;
            type UserSet = Set;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("UserList").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("UserMap").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("UserSet").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn result_type_aliases() {
        // Define the types that we alias to (public to match alias visibility)
        Test::new(
            r#"module Test
            public struct Either {}
            public struct Optional {}
            struct ErrorType {}
            public type Result = Either;
            public type Maybe = Optional;
            type Error = ErrorType;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Result").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Maybe").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Error").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn chained_aliases() {
        Test::new(
            r#"module Test
            type Base = Int;
            type Derived = Base;
            type Final = Derived;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Base").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Derived").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Final").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn multiple_aliases_same_target() {
        Test::new(
            r#"module Test
            type Alias1 = Int;
            type Alias2 = Int;
            type Alias3 = Int;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Alias1").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Alias2").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Alias3").is(SymbolKind::TypeAlias));
    }
}

mod mixed_features {
    use super::*;

    #[test]
    fn type_alias_with_imports() {
        Test::with_files(&[
            ("collections.ks", "module System.Collections\npublic struct Array {}"),
            (
                "graphics.ks",
                r#"module Graphics
                import System.Collections
                public struct RGB {}
                struct Point2D {}
                public type Color = RGB;
                type Position = Point2D;"#,
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("Color").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Position").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn type_alias_with_classes() {
        Test::new(
            r#"module Graphics
            public struct RGB {}
            struct Point2D {}
            struct Coordinate {}
            public type Color = RGB;
            type Position = Point2D;
            struct Shape {}
            type Point = Coordinate;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Color").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Position").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Shape").is(SymbolKind::Struct))
        .expect(Symbol::new("Point").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn interleaved_declarations() {
        Test::new(
            r#"module Graphics
            struct Coordinate {}
            struct Degree {}
            type Point = Coordinate;
            struct Triangle {}
            type Angle = Degree;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Point").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Triangle").is(SymbolKind::Struct))
        .expect(Symbol::new("Angle").is(SymbolKind::TypeAlias));
    }
}


mod whitespace {
    use super::*;

    #[test]
    fn standard_formatting() {
        Test::new("module Test\ntype Standard = Int;")
            .expect(Compiles)
            .expect(Symbol::new("Standard").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn extra_spaces() {
        Test::new("module Test\ntype   ExtraSpaces   =   String   ;")
            .expect(Compiles)
            .expect(Symbol::new("ExtraSpaces").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn minimal_spacing() {
        Test::new("module Test\ntype Minimal=Bool;")
            .expect(Compiles)
            .expect(Symbol::new("Minimal").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn blank_lines_between_declarations() {
        Test::new(
            r#"module Test

            type WithBlankLines = Int;


            type MoreBlankLines = String;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("WithBlankLines").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("MoreBlankLines").is(SymbolKind::TypeAlias));
    }
}

mod cycle_detection {
    use super::*;

    #[test]
    fn direct_self_reference() {
        Test::new("module Test\ntype A = A;")
            .expect(HasError("circular type alias"));
    }

    #[test]
    fn indirect_two_way_cycle() {
        Test::new(
            r#"module Test
            type A = B;
            type B = A;
        "#,
        )
        .expect(HasError("circular type alias"));
    }

    #[test]
    fn three_way_cycle() {
        Test::new(
            r#"module Test
            type A = B;
            type B = C;
            type C = A;
        "#,
        )
        .expect(HasError("circular type alias"));
    }

    #[test]
    fn longer_cycle() {
        Test::new(
            r#"module Test
            type A = B;
            type B = C;
            type C = D;
            type D = E;
            type E = A;
        "#,
        )
        .expect(HasError("circular type alias"));
    }

    #[test]
    fn valid_chain_no_cycle() {
        Test::new(
            r#"module Test
            type A = B;
            type B = Int;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("A").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("B").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn valid_longer_chain() {
        Test::new(
            r#"module Test
            type A = B;
            type B = C;
            type C = D;
            type D = Int;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("A").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("B").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("C").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("D").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn multiple_independent_chains() {
        Test::new(
            r#"module Test
            type A = B;
            type B = Int;
            type X = Y;
            type Y = String;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("A").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("B").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("X").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Y").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn cycle_in_tuple() {
        // Note: Cycles through tuples still create a cycle in the type alias graph
        // The cycle detection now catches this, even though B appears inside a tuple
        Test::new(
            r#"module Test
            type A = (B, Int);
            type B = A;
        "#,
        )
        .expect(HasError("circular type alias"));
    }

    // Note: Function type syntax tests are commented out as they may not be fully supported yet
    // #[test]
    // fn cycle_in_function_params() {
    //     Test::new(
    //         r#"module Test
    //         type A = (B) => Int;
    //         type B = A;
    //     "#,
    //     )
    //     .expect(Compiles)
    //     .expect(Symbol::new("A").is(SymbolKind::TypeAlias))
    //     .expect(Symbol::new("B").is(SymbolKind::TypeAlias));
    // }
    //
    // #[test]
    // fn cycle_in_function_return() {
    //     Test::new(
    //         r#"module Test
    //         type A = () => B;
    //         type B = A;
    //     "#,
    //     )
    //     .expect(Compiles)
    //     .expect(Symbol::new("A").is(SymbolKind::TypeAlias))
    //     .expect(Symbol::new("B").is(SymbolKind::TypeAlias));
    // }

    #[test]
    fn valid_recursive_structure_different_types() {
        // This should compile because A and B point to different concrete types
        Test::new(
            r#"module Test
            type A = (Int, B);
            type B = String;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("A").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("B").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn mixed_valid_and_invalid() {
        // First two are valid, third creates a cycle
        Test::new(
            r#"module Test
            type Valid1 = Int;
            type Valid2 = String;
            type Cycle1 = Cycle2;
            type Cycle2 = Cycle1;
        "#,
        )
        .expect(HasError("circular type alias"));
    }
}

mod unresolved_types {
    use super::*;

    // These tests document expected behavior for unresolved type diagnostics.
    // The infrastructure (resolve_type_with_diagnostics) is in place but requires
    // type parameters to be included in scope resolution first.

    #[test]
    fn type_alias_to_unknown() {
        Test::new(
            r#"module Test
            type Foo = Unknown;
        "#,
        )
        .expect(HasError("cannot find type"));
    }

    #[test]
    fn type_alias_to_unknown_in_tuple() {
        Test::new(
            r#"module Test
            type Foo = (Int, Unknown, String);
        "#,
        )
        .expect(HasError("cannot find type"));
    }

    #[test]
    fn field_with_unknown_type() {
        Test::new(
            r#"module Test
            struct Foo {
                let bar: Unknown
            }
        "#,
        )
        .expect(HasError("cannot find type"));
    }

    #[test]
    fn function_with_unknown_return_type() {
        Test::new(
            r#"module Test
            func foo() -> Unknown { }
        "#,
        )
        .expect(HasError("cannot find type"));
    }

    #[test]
    fn function_with_unknown_param_type() {
        Test::new(
            r#"module Test
            func foo(x: Unknown) { }
        "#,
        )
        .expect(HasError("cannot find type"));
    }

    #[test]
    fn generic_with_unknown_type_arg() {
        Test::new(
            r#"module Test
            struct Box[T] { }
            type Foo = Box[Unknown];
        "#,
        )
        .expect(HasError("cannot find type"));
    }
}
