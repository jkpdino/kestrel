use kestrel_test_suite::*;

mod basic {
    use super::*;

    #[test]
    fn empty_struct() {
        Test::new("module Test\nstruct Foo {}")
            .expect(Compiles)
            .expect(Symbol::new("Foo").is(SymbolKind::Struct));
    }

    #[test]
    fn public_struct() {
        Test::new("module Test\npublic struct Bar {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Bar")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Public)),
            );
    }

    #[test]
    fn private_struct() {
        Test::new("module Test\nprivate struct Baz {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Baz")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Private)),
            );
    }

    #[test]
    fn internal_struct() {
        Test::new("module Test\ninternal struct Qux {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Qux")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Internal)),
            );
    }

    #[test]
    fn fileprivate_struct() {
        Test::new("module Test\nfileprivate struct Quux {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Quux")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Fileprivate)),
            );
    }

    #[test]
    fn default_visibility_is_internal() {
        Test::new("module Test\nstruct DefaultVis {}")
            .expect(Compiles)
            .expect(
                Symbol::new("DefaultVis")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Internal)),
            );
    }

    #[test]
    fn multiple_structs() {
        Test::new(
            r#"module Test
            struct First {}
            struct Second {}
            struct Third {}
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("First").is(SymbolKind::Struct))
        .expect(Symbol::new("Second").is(SymbolKind::Struct))
        .expect(Symbol::new("Third").is(SymbolKind::Struct));
    }
}

mod nested {
    use super::*;

    #[test]
    fn nested_struct() {
        Test::new(
            r#"module Test
            struct Outer {
                struct Inner {}
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Outer").is(SymbolKind::Struct))
        .expect(Symbol::new("Inner").is(SymbolKind::Struct));
    }

    #[test]
    fn deeply_nested_structs() {
        Test::new(
            r#"module Test
            struct Level1 {
                struct Level2 {
                    struct Level3 {}
                }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Level1").is(SymbolKind::Struct))
        .expect(Symbol::new("Level2").is(SymbolKind::Struct))
        .expect(Symbol::new("Level3").is(SymbolKind::Struct));
    }
}
