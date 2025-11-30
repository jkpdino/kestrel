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
            .expect(HasError("static modifier is only allowed inside struct, class, or protocol"));
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
}
