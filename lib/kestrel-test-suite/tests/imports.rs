use kestrel_test_suite::*;

mod basic {
    use super::*;

    #[test]
    fn import_module() {
        Test::with_files(&[
            ("library.ks", "module Library\npublic struct PublicClass {}"),
            ("consumer.ks", "module Consumer\nimport Library\nstruct UsesPublic {}"),
        ])
        .expect(Compiles)
        .expect(Symbol::new("PublicClass").is(SymbolKind::Struct))
        .expect(Symbol::new("UsesPublic").is(SymbolKind::Struct));
    }

    #[test]
    fn import_specific_items() {
        Test::with_files(&[
            (
                "library.ks",
                "module Library\npublic struct PublicClass {}\npublic type PublicAlias = PublicClass;",
            ),
            (
                "consumer.ks",
                "module SpecificImport\nimport Library.(PublicClass, PublicAlias)\nstruct MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyClass").is(SymbolKind::Struct));
    }

    #[test]
    fn import_with_module_alias() {
        Test::with_files(&[
            ("library.ks", "module Library\npublic struct PublicClass {}"),
            (
                "consumer.ks",
                "module AliasedImport\nimport Library as Lib\nstruct MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyClass").is(SymbolKind::Struct));
    }

    #[test]
    fn import_with_item_alias() {
        Test::with_files(&[
            ("library.ks", "module Library\npublic struct PublicClass {}"),
            (
                "consumer.ks",
                "module AliasedImport\nimport Library.(PublicClass as PC)\nstruct MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyClass").is(SymbolKind::Struct));
    }
}

mod nested_modules {
    use super::*;

    #[test]
    fn import_top_level_module() {
        Test::with_files(&[
            ("math.ks", "module Math\npublic struct Vector {}\npublic struct Matrix {}"),
            ("consumer.ks", "module NestedConsumer\nimport Math\nstruct MyApp {}"),
        ])
        .expect(Compiles)
        .expect(Symbol::new("Vector").is(SymbolKind::Struct))
        .expect(Symbol::new("MyApp").is(SymbolKind::Struct));
    }

    #[test]
    fn import_nested_module() {
        Test::with_files(&[
            (
                "math_geometry.ks",
                "module Math.Geometry\npublic struct Point {}\npublic struct Circle {}",
            ),
            (
                "consumer.ks",
                "module NestedConsumer\nimport Math.Geometry\nstruct MyApp {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("Point").is(SymbolKind::Struct))
        .expect(Symbol::new("Circle").is(SymbolKind::Struct));
    }

    #[test]
    fn import_specific_from_nested() {
        Test::with_files(&[
            (
                "math_geometry.ks",
                "module Math.Geometry\npublic struct Point {}\npublic struct Line {}\npublic struct Circle {}",
            ),
            (
                "consumer.ks",
                "module NestedConsumer\nimport Math.Geometry.(Point, Circle)\nstruct MyApp {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyApp").is(SymbolKind::Struct));
    }

    #[test]
    fn import_nested_with_alias() {
        Test::with_files(&[
            (
                "math_algebra.ks",
                "module Math.Algebra\npublic struct Polynomial {}\npublic struct Equation {}",
            ),
            (
                "consumer.ks",
                "module NestedConsumer\nimport Math.Algebra as Alg\nstruct MyApp {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyApp").is(SymbolKind::Struct));
    }
}

mod visibility {
    use super::*;

    #[test]
    fn import_public_class() {
        Test::with_files(&[
            ("library.ks", "module Library\npublic struct PublicClass {}"),
            ("consumer.ks", "module Consumer\nimport Library\nstruct UsesPublic {}"),
        ])
        .expect(Compiles)
        .expect(Symbol::new("PublicClass").is(SymbolKind::Struct));
    }

    #[test]
    fn import_public_type_alias() {
        Test::with_files(&[
            (
                "library.ks",
                "module Library\npublic struct PublicClass {}\npublic type PublicAlias = PublicClass;",
            ),
            (
                "consumer.ks",
                "module Consumer\nimport Library.(PublicAlias)\nstruct MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("PublicAlias").is(SymbolKind::TypeAlias));
    }

    #[test]
    fn can_see_internal_in_same_module() {
        Test::with_files(&[
            (
                "internal_lib.ks",
                "module InternalLib\ninternal struct InternalClass {}\npublic struct PublicClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("InternalClass").is(SymbolKind::Struct))
        .expect(Symbol::new("PublicClass").is(SymbolKind::Struct));
    }
}

mod conflicts {
    use super::*;


    #[test]
    fn resolve_conflict_with_aliases() {
        Test::with_files(&[
            ("module_a.ks", "module ModuleA\npublic struct Widget {}\npublic struct Helper {}"),
            ("module_b.ks", "module ModuleB\npublic struct Widget {}\npublic struct Utility {}"),
            (
                "consumer.ks",
                "module AliasedConsumer\nimport ModuleA.(Widget as WidgetA)\nimport ModuleB.(Widget as WidgetB)\nstruct MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyClass").is(SymbolKind::Struct));
    }
}

