use kestrel_test_suite::*;

mod function_body {
    use super::*;

    #[test]
    fn function_with_body_compiles() {
        Test::new("module Test\nfunc hasBody() { }")
            .expect(Compiles)
            .expect(Symbol::new("hasBody").is(SymbolKind::Function));
    }

    #[test]
    fn function_without_body_errors() {
        Test::new("module Test\nfunc missingBody() -> Int")
            .expect(HasError("requires a body"));
    }

    #[test]
    fn multiple_functions_one_missing_body() {
        Test::new(
            r#"module Test
            func valid() { }
            func invalid() -> Int
        "#,
        )
        .expect(HasError("'invalid' requires a body"));
    }
}

mod protocol_methods {
    use super::*;

    #[test]
    fn protocol_method_without_body_compiles() {
        Test::new(
            r#"module Test
            protocol Printable {
                func print() -> ()
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Printable").is(SymbolKind::Protocol))
        .expect(Symbol::new("print").is(SymbolKind::Function));
    }

    #[test]
    fn protocol_method_with_body_errors() {
        Test::new(
            r#"module Test
            protocol Drawable {
                func draw() -> () { }
            }
        "#,
        )
        .expect(HasError("cannot have a body"));
    }

    #[test]
    fn protocol_with_multiple_methods() {
        Test::new(
            r#"module Test
            protocol Hashable {
                func hash() -> Int
                func equals(other to: Int) -> Bool
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Hashable").is(SymbolKind::Protocol))
        .expect(Symbol::new("hash").is(SymbolKind::Function))
        .expect(Symbol::new("equals").is(SymbolKind::Function));
    }
}

mod mixed {
    use super::*;

    #[test]
    fn protocol_and_regular_function() {
        Test::new(
            r#"module Test
            protocol Runnable {
                func run() -> ()
            }

            func execute() { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Runnable").is(SymbolKind::Protocol))
        .expect(Symbol::new("run").is(SymbolKind::Function))
        .expect(Symbol::new("execute").is(SymbolKind::Function));
    }
}

mod static_context {
    use super::*;

    #[test]
    fn static_function_in_struct_compiles() {
        Test::new(
            r#"module Test
            struct Counter {
                static func create() { }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("create").is(SymbolKind::Function));
    }

    #[test]
    fn static_function_in_protocol_compiles() {
        Test::new(
            r#"module Test
            protocol Factory {
                static func create() -> ()
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("create").is(SymbolKind::Function));
    }

    #[test]
    fn static_function_at_module_level_errors() {
        Test::new("module Test\nstatic func topLevel() { }")
            .expect(HasError("static modifier is only allowed inside struct or protocol"));
    }
}

mod duplicate_symbol {
    use super::*;

    #[test]
    fn duplicate_struct_errors() {
        Test::new(
            r#"module Test
            struct Foo { }
            struct Foo { }
        "#,
        )
        .expect(HasError("duplicate type 'Foo'"));
    }

    #[test]
    fn duplicate_protocol_errors() {
        Test::new(
            r#"module Test
            protocol Bar { }
            protocol Bar { }
        "#,
        )
        .expect(HasError("duplicate type 'Bar'"));
    }

    #[test]
    fn struct_and_protocol_same_name_errors() {
        Test::new(
            r#"module Test
            struct Thing { }
            protocol Thing { }
        "#,
        )
        .expect(HasError("duplicate type 'Thing'"));
    }

    #[test]
    fn different_types_different_names_compiles() {
        Test::new(
            r#"module Test
            struct Foo { }
            protocol Bar { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_overloading_allowed() {
        Test::new(
            r#"module Test
            func process() { }
            func process(x: Int) { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn duplicate_type_alias_errors() {
        Test::new(
            r#"module Test
            type Alias = Int;
            type Alias = String;
        "#,
        )
        .expect(HasError("duplicate type 'Alias'"));
    }

    #[test]
    fn duplicate_field_same_struct_errors() {
        Test::new(
            r#"module Test
            struct Record {
                let name: String
                let name: Int
            }
        "#,
        )
        .expect(HasError("duplicate member 'name'"));
    }

    #[test]
    fn same_field_different_structs_compiles() {
        Test::new(
            r#"module Test
            struct First {
                let value: Int
            }
            struct Second {
                let value: String
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod visibility_consistency {
    use super::*;

    // These tests document expected behavior for visibility consistency validation.
    // Some are ignored because the validation is not yet fully implemented.

    #[test]
    fn public_field_with_private_type_errors() {
        Test::new(
            r#"module Test
            private struct PrivateType { }
            public struct Container {
                public let value: PrivateType
            }
        "#,
        )
        .expect(HasError("exposes private type"));
    }

    #[test]
    fn public_function_with_private_return_errors() {
        Test::new(
            r#"module Test
            private struct Secret { }
            public func getSecret() -> Secret { }
        "#,
        )
        .expect(HasError("exposes private type"));
    }

    #[test]
    fn public_function_with_private_param_errors() {
        Test::new(
            r#"module Test
            private struct Secret { }
            public func process(s: Secret) { }
        "#,
        )
        .expect(HasError("exposes private type"));
    }

    #[test]
    fn internal_function_with_private_return_compiles() {
        // Internal function can use private types within same scope
        Test::new(
            r#"module Test
            private struct Internal { }
            func helper() -> Internal { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn public_type_alias_with_private_underlying_errors() {
        Test::new(
            r#"module Test
            private struct Hidden { }
            public type Exposed = Hidden;
        "#,
        )
        .expect(HasError("exposes private type"));
    }

    #[test]
    fn protocol_method_with_private_param_in_public_protocol_errors() {
        Test::new(
            r#"module Test
            private struct Secret { }
            public protocol Handler {
                func handle(s: Secret) -> ()
            }
        "#,
        )
        .expect(HasError("exposes private type"));
    }
}

mod module_resolution {
    use super::*;

    #[test]
    fn submodule_type_resolution() {
        Test::new(
            r#"module Outer.Inner
            struct NestedType { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("NestedType").is(SymbolKind::Struct));
    }

    #[test]
    fn deeply_nested_module() {
        Test::new(
            r#"module A.B.C.D
            struct DeepType { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("DeepType").is(SymbolKind::Struct));
    }
}
