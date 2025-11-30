use kestrel_test_suite::*;

mod basic {
    use super::*;

    #[test]
    fn import_module() {
        Test::with_files(&[
            ("library.ks", "module Library\npublic class PublicClass {}"),
            ("consumer.ks", "module Consumer\nimport Library\nclass UsesPublic {}"),
        ])
        .expect(Compiles)
        .expect(Symbol::new("PublicClass").is(SymbolKind::Class))
        .expect(Symbol::new("UsesPublic").is(SymbolKind::Class));
    }

    #[test]
    fn import_specific_items() {
        Test::with_files(&[
            (
                "library.ks",
                "module Library\npublic class PublicClass {}\npublic type PublicAlias = PublicClass;",
            ),
            (
                "consumer.ks",
                "module SpecificImport\nimport Library.(PublicClass, PublicAlias)\nclass MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyClass").is(SymbolKind::Class));
    }

    #[test]
    fn import_with_module_alias() {
        Test::with_files(&[
            ("library.ks", "module Library\npublic class PublicClass {}"),
            (
                "consumer.ks",
                "module AliasedImport\nimport Library as Lib\nclass MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyClass").is(SymbolKind::Class));
    }

    #[test]
    fn import_with_item_alias() {
        Test::with_files(&[
            ("library.ks", "module Library\npublic class PublicClass {}"),
            (
                "consumer.ks",
                "module AliasedImport\nimport Library.(PublicClass as PC)\nclass MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyClass").is(SymbolKind::Class));
    }
}

mod nested_modules {
    use super::*;

    #[test]
    fn import_top_level_module() {
        Test::with_files(&[
            ("math.ks", "module Math\npublic class Vector {}\npublic class Matrix {}"),
            ("consumer.ks", "module NestedConsumer\nimport Math\nclass MyApp {}"),
        ])
        .expect(Compiles)
        .expect(Symbol::new("Vector").is(SymbolKind::Class))
        .expect(Symbol::new("MyApp").is(SymbolKind::Class));
    }

    #[test]
    fn import_nested_module() {
        Test::with_files(&[
            (
                "math_geometry.ks",
                "module Math.Geometry\npublic class Point {}\npublic class Circle {}",
            ),
            (
                "consumer.ks",
                "module NestedConsumer\nimport Math.Geometry\nclass MyApp {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("Point").is(SymbolKind::Class))
        .expect(Symbol::new("Circle").is(SymbolKind::Class));
    }

    #[test]
    fn import_specific_from_nested() {
        Test::with_files(&[
            (
                "math_geometry.ks",
                "module Math.Geometry\npublic class Point {}\npublic class Line {}\npublic class Circle {}",
            ),
            (
                "consumer.ks",
                "module NestedConsumer\nimport Math.Geometry.(Point, Circle)\nclass MyApp {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyApp").is(SymbolKind::Class));
    }

    #[test]
    fn import_nested_with_alias() {
        Test::with_files(&[
            (
                "math_algebra.ks",
                "module Math.Algebra\npublic class Polynomial {}\npublic class Equation {}",
            ),
            (
                "consumer.ks",
                "module NestedConsumer\nimport Math.Algebra as Alg\nclass MyApp {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyApp").is(SymbolKind::Class));
    }
}

mod visibility {
    use super::*;

    #[test]
    fn import_public_class() {
        Test::with_files(&[
            ("library.ks", "module Library\npublic class PublicClass {}"),
            ("consumer.ks", "module Consumer\nimport Library\nclass UsesPublic {}"),
        ])
        .expect(Compiles)
        .expect(Symbol::new("PublicClass").is(SymbolKind::Class));
    }

    #[test]
    fn import_public_type_alias() {
        Test::with_files(&[
            (
                "library.ks",
                "module Library\npublic class PublicClass {}\npublic type PublicAlias = PublicClass;",
            ),
            (
                "consumer.ks",
                "module Consumer\nimport Library.(PublicAlias)\nclass MyClass {}",
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
                "module InternalLib\ninternal class InternalClass {}\npublic class PublicClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("InternalClass").is(SymbolKind::Class))
        .expect(Symbol::new("PublicClass").is(SymbolKind::Class));
    }
}

mod conflicts {
    use super::*;


    #[test]
    fn resolve_conflict_with_aliases() {
        Test::with_files(&[
            ("module_a.ks", "module ModuleA\npublic class Widget {}\npublic class Helper {}"),
            ("module_b.ks", "module ModuleB\npublic class Widget {}\npublic class Utility {}"),
            (
                "consumer.ks",
                "module AliasedConsumer\nimport ModuleA.(Widget as WidgetA)\nimport ModuleB.(Widget as WidgetB)\nclass MyClass {}",
            ),
        ])
        .expect(Compiles)
        .expect(Symbol::new("MyClass").is(SymbolKind::Class));
    }
}

