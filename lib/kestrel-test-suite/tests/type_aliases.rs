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
        Test::new("module Test\nfileprivate type FilePrivateAlias = Char;")
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
            type Result = Either;
            type Maybe = Optional;
            type List = Array;
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
        Test::new("module Test\ntype CamelCaseAlias = SomeType;")
            .expect(Compiles)
            .expect(Symbol::new("CamelCaseAlias").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn snake_case_alias() {
        Test::new("module Test\ntype snake_case_alias = AnotherType;")
            .expect(Compiles)
            .expect(Symbol::new("snake_case_alias").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn screaming_snake_alias() {
        Test::new("module Test\ntype SCREAMING_SNAKE = YetAnotherType;")
            .expect(Compiles)
            .expect(Symbol::new("SCREAMING_SNAKE").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn alias_with_numbers() {
        Test::new("module Test\ntype MixedCase123 = TypeWithNumbers;")
            .expect(Compiles)
            .expect(Symbol::new("MixedCase123").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn single_character_alias() {
        Test::new("module Test\ntype A = Foo;\ntype B = Bar;")
            .expect(Compiles)
            .expect(Symbol::new("A").is(SymbolKind::TypeAlias))
            .expect(Symbol::new("B").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn alias_with_underscores() {
        Test::new(
            r#"module Test
            type _leading = LeadingUnderscore;
            type trailing_ = TrailingUnderscore;
            type _both_ = BothUnderscore;
            type mid_dle = MiddleUnderscore;
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
            type Type1 = First;
            type Alias0 = Zero;
            type Map2D = TwoDimensionalMap;
            type Vector3D = ThreeDimensionalVector;
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
        Test::new(
            r#"module Test
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
        Test::new(
            r#"module Test
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
            type Base = Foundation;
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
            type Alias1 = TargetType;
            type Alias2 = TargetType;
            type Alias3 = TargetType;
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
            ("collections.ks", "module System.Collections\npublic class Array {}"),
            (
                "graphics.ks",
                "module Graphics\nimport System.Collections\npublic type Color = RGB;\ntype Position = Point2D;",
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
            public type Color = RGB;
            type Position = Point2D;
            class Shape {}
            type Point = Coordinate;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Color").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Position").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Shape").is(SymbolKind::Class))
        .expect(Symbol::new("Point").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn interleaved_declarations() {
        Test::new(
            r#"module Graphics
            type Point = Coordinate;
            class Triangle {}
            type Angle = Degree;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Point").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("Triangle").is(SymbolKind::Class))
        .expect(Symbol::new("Angle").is(SymbolKind::TypeAlias));
    }
}


mod whitespace {
    use super::*;

    #[test]
    fn standard_formatting() {
        Test::new("module Test\ntype Standard = Normal;")
            .expect(Compiles)
            .expect(Symbol::new("Standard").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn extra_spaces() {
        Test::new("module Test\ntype   ExtraSpaces   =   Spaced   ;")
            .expect(Compiles)
            .expect(Symbol::new("ExtraSpaces").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn minimal_spacing() {
        Test::new("module Test\ntype Minimal=Compact;")
            .expect(Compiles)
            .expect(Symbol::new("Minimal").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn blank_lines_between_declarations() {
        Test::new(
            r#"module Test

            type WithBlankLines = Type1;


            type MoreBlankLines = Type2;
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("WithBlankLines").is(SymbolKind::TypeAlias))
        .expect(Symbol::new("MoreBlankLines").is(SymbolKind::TypeAlias));
    }
}
