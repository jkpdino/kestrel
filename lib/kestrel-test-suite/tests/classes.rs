use kestrel_test_suite::*;

mod basic {
    use super::*;

    #[test]
    fn empty_class() {
        Test::new("module Test\nclass Empty {}")
            .expect(Compiles)
            .expect(Symbol::new("Empty").is(SymbolKind::Class));
    }

    #[test]
    fn public_class() {
        Test::new("module Test\npublic class Public {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Public")
                    .is(SymbolKind::Class)
                    .has(Behavior::Visibility(Visibility::Public)),
            );
    }

    #[test]
    fn private_class() {
        Test::new("module Test\nprivate class Private {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Private")
                    .is(SymbolKind::Class)
                    .has(Behavior::Visibility(Visibility::Private)),
            );
    }

    #[test]
    fn internal_class() {
        Test::new("module Test\ninternal class Internal {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Internal")
                    .is(SymbolKind::Class)
                    .has(Behavior::Visibility(Visibility::Internal)),
            );
    }

    #[test]
    fn fileprivate_class() {
        Test::new("module Test\nfileprivate class Fileprivate {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Fileprivate")
                    .is(SymbolKind::Class)
                    .has(Behavior::Visibility(Visibility::Fileprivate)),
            );
    }

    #[test]
    fn default_visibility_is_internal() {
        Test::new("module Test\nclass DefaultVis {}")
            .expect(Compiles)
            .expect(
                Symbol::new("DefaultVis")
                    .is(SymbolKind::Class)
                    .has(Behavior::Visibility(Visibility::Internal)),
            );
    }

    #[test]
    fn multiple_classes() {
        Test::new(
            r#"module Test
            class First {}
            class Second {}
            class Third {}
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("First").is(SymbolKind::Class))
        .expect(Symbol::new("Second").is(SymbolKind::Class))
        .expect(Symbol::new("Third").is(SymbolKind::Class));
    }
}

mod naming {
    use super::*;

    #[test]
    fn single_character_class() {
        Test::new("module Test\nclass A {}")
            .expect(Compiles)
            .expect(Symbol::new("A").is(SymbolKind::Class));
    }

    #[test]
    fn class_with_underscores() {
        Test::new(
            r#"module Test
            class _LeadingUnderscore {}
            class TrailingUnderscore_ {}
            class _BothUnderscores_ {}
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("_LeadingUnderscore").is(SymbolKind::Class))
        .expect(Symbol::new("TrailingUnderscore_").is(SymbolKind::Class))
        .expect(Symbol::new("_BothUnderscores_").is(SymbolKind::Class));
    }

    #[test]
    fn class_with_numbers() {
        Test::new(
            r#"module Test
            class Class1 {}
            class MyClass2000 {}
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Class1").is(SymbolKind::Class))
        .expect(Symbol::new("MyClass2000").is(SymbolKind::Class));
    }
}

mod visibility_patterns {
    use super::*;

    #[test]
    fn mixed_visibility() {
        Test::new(
            r#"module Test
            public class PublicOne {}
            private class PrivateOne {}
            internal class InternalOne {}
            fileprivate class FileprivateOne {}
            class DefaultOne {}
        "#,
        )
        .expect(Compiles)
        .expect(
            Symbol::new("PublicOne")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Public)),
        )
        .expect(
            Symbol::new("PrivateOne")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Private)),
        )
        .expect(
            Symbol::new("InternalOne")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Internal)),
        )
        .expect(
            Symbol::new("FileprivateOne")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Fileprivate)),
        )
        .expect(
            Symbol::new("DefaultOne")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Internal)),
        );
    }
}

mod nested {
    use super::*;

    #[test]
    fn basic_nested_class() {
        Test::new(
            r#"module Test
            class Outer {
                class Inner {}
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Outer").is(SymbolKind::Class))
        .expect(Symbol::new("Inner").is(SymbolKind::Class));
    }

    #[test]
    fn multiple_levels_of_nesting() {
        Test::new(
            r#"module Test
            class Level1 {
                class Level2 {
                    class Level3 {}
                }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Level1").is(SymbolKind::Class))
        .expect(Symbol::new("Level2").is(SymbolKind::Class))
        .expect(Symbol::new("Level3").is(SymbolKind::Class));
    }

    #[test]
    fn multiple_nested_classes_in_one_parent() {
        Test::new(
            r#"module Test
            class Container {
                class FirstNested {}
                class SecondNested {}
                class ThirdNested {}
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Container").is(SymbolKind::Class))
        .expect(Symbol::new("FirstNested").is(SymbolKind::Class))
        .expect(Symbol::new("SecondNested").is(SymbolKind::Class))
        .expect(Symbol::new("ThirdNested").is(SymbolKind::Class));
    }

    #[test]
    fn nested_classes_with_visibility() {
        Test::new(
            r#"module Test
            public class PublicOuter {
                private class PrivateInner {}
                public class PublicInner {}
                internal class InternalInner {}
            }
        "#,
        )
        .expect(Compiles)
        .expect(
            Symbol::new("PublicOuter")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Public)),
        )
        .expect(
            Symbol::new("PrivateInner")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Private)),
        )
        .expect(
            Symbol::new("PublicInner")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Public)),
        )
        .expect(
            Symbol::new("InternalInner")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Internal)),
        );
    }

    #[test]
    fn deep_nesting() {
        Test::new(
            r#"module Test
            class A {
                class B {
                    class C {
                        class D {
                            class E {}
                        }
                    }
                }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("A").is(SymbolKind::Class))
        .expect(Symbol::new("B").is(SymbolKind::Class))
        .expect(Symbol::new("C").is(SymbolKind::Class))
        .expect(Symbol::new("D").is(SymbolKind::Class))
        .expect(Symbol::new("E").is(SymbolKind::Class));
    }

    #[test]
    fn mixed_nested_and_non_nested() {
        Test::new(
            r#"module Test
            class First {}

            class Second {
                class NestedInSecond {}
            }

            class Third {}

            class Fourth {
                class NestedOne {}
                class NestedTwo {
                    class DeeplyNested {}
                }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("First").is(SymbolKind::Class))
        .expect(Symbol::new("Second").is(SymbolKind::Class))
        .expect(Symbol::new("NestedInSecond").is(SymbolKind::Class))
        .expect(Symbol::new("Third").is(SymbolKind::Class))
        .expect(Symbol::new("Fourth").is(SymbolKind::Class))
        .expect(Symbol::new("NestedOne").is(SymbolKind::Class))
        .expect(Symbol::new("NestedTwo").is(SymbolKind::Class))
        .expect(Symbol::new("DeeplyNested").is(SymbolKind::Class));
    }

    #[test]
    fn simple_nested_example() {
        Test::new(
            r#"module Test
            private class WithNested {
                class Nested {}
            }
        "#,
        )
        .expect(Compiles)
        .expect(
            Symbol::new("WithNested")
                .is(SymbolKind::Class)
                .has(Behavior::Visibility(Visibility::Private)),
        )
        .expect(Symbol::new("Nested").is(SymbolKind::Class));
    }
}
