use kestrel_test_suite::*;

mod basic {
    use super::*;

    #[test]
    fn empty_function() {
        Test::new("module Test\nfunc empty() { }")
            .expect(Compiles)
            .expect(Symbol::new("empty").is(SymbolKind::Function));
    }

    #[test]
    fn function_with_return_type() {
        Test::new("module Test\nfunc getValue() -> Int { }")
            .expect(Compiles)
            .expect(Symbol::new("getValue").is(SymbolKind::Function));
    }

    #[test]
    fn function_with_parameters() {
        Test::new("module Test\nfunc add(a: Int, b: Int) -> Int { }")
            .expect(Compiles)
            .expect(Symbol::new("add").is(SymbolKind::Function));
    }

    #[test]
    fn public_function() {
        Test::new("module Test\npublic func publicFn() { }")
            .expect(Compiles)
            .expect(
                Symbol::new("publicFn")
                    .is(SymbolKind::Function)
                    .has(Behavior::Visibility(Visibility::Public)),
            );
    }

    #[test]
    fn private_function() {
        Test::new("module Test\nprivate func privateFn() { }")
            .expect(Compiles)
            .expect(
                Symbol::new("privateFn")
                    .is(SymbolKind::Function)
                    .has(Behavior::Visibility(Visibility::Private)),
            );
    }

    #[test]
    fn static_function_in_struct() {
        Test::new(
            r#"module Test
            struct Counter {
                static func staticFn() { }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("staticFn").is(SymbolKind::Function));
    }
}

mod overloading {
    use super::*;

    #[test]
    fn overload_by_parameter_count() {
        Test::new(
            r#"module Test
            func process() { }
            func process(x: Int) { }
            func process(x: Int, y: Int) { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn overload_by_parameter_type() {
        Test::new(
            r#"module Test
            func convert(x: Int) -> String { }
            func convert(x: Float) -> String { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn overload_by_label() {
        Test::new(
            r#"module Test
            func send(to recipient: String) { }
            func send(from sender: String) { }
        "#,
        )
        .expect(Compiles);
    }
}

mod in_structs {
    use super::*;

    #[test]
    fn method_in_struct() {
        Test::new(
            r#"module Test
            struct Counter {
                func increment() { }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Counter").is(SymbolKind::Struct))
        .expect(Symbol::new("increment").is(SymbolKind::Function));
    }

    #[test]
    fn multiple_methods() {
        Test::new(
            r#"module Test
            struct Calculator {
                func add(a: Int, b: Int) -> Int { }
                func subtract(a: Int, b: Int) -> Int { }
                func multiply(a: Int, b: Int) -> Int { }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Calculator").is(SymbolKind::Struct))
        .expect(Symbol::new("add").is(SymbolKind::Function))
        .expect(Symbol::new("subtract").is(SymbolKind::Function))
        .expect(Symbol::new("multiply").is(SymbolKind::Function));
    }
}
