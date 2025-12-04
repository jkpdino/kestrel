use kestrel_test_suite::*;

mod basic {
    use super::*;

    #[test]
    fn empty_protocol() {
        Test::new("module Test\nprotocol Drawable { }")
            .expect(Compiles)
            .expect(Symbol::new("Drawable").is(SymbolKind::Protocol));
    }

    #[test]
    fn public_protocol() {
        Test::new("module Test\npublic protocol Equatable { }")
            .expect(Compiles)
            .expect(
                Symbol::new("Equatable")
                    .is(SymbolKind::Protocol)
                    .has(Behavior::Visibility(Visibility::Public)),
            );
    }

    #[test]
    fn protocol_with_method() {
        Test::new(
            r#"module Test
            protocol Hashable {
                func hash() -> Int
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Hashable").is(SymbolKind::Protocol))
        .expect(Symbol::new("hash").is(SymbolKind::Function));
    }

    #[test]
    fn protocol_with_multiple_methods() {
        Test::new(
            r#"module Test
            protocol Comparable {
                func lessThan(other: Self) -> Bool
                func greaterThan(other: Self) -> Bool
                func equals(other: Self) -> Bool
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Comparable").is(SymbolKind::Protocol))
        .expect(Symbol::new("lessThan").is(SymbolKind::Function))
        .expect(Symbol::new("greaterThan").is(SymbolKind::Function))
        .expect(Symbol::new("equals").is(SymbolKind::Function));
    }
}

mod conformance {
    use super::*;

    #[test]
    fn struct_with_single_conformance() {
        Test::new(
            r#"module Test
            protocol Drawable { }
            struct Point: Drawable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Drawable").is(SymbolKind::Protocol))
        .expect(Symbol::new("Point").is(SymbolKind::Struct));
    }

    #[test]
    fn struct_with_multiple_conformances() {
        Test::new(
            r#"module Test
            protocol Drawable { }
            protocol Equatable { }
            struct Point: Drawable, Equatable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Point").is(SymbolKind::Struct));
    }

    #[test]
    fn struct_with_conformance_and_type_params() {
        Test::new(
            r#"module Test
            protocol Container[T] { }
            struct Box[T]: Container[T] { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Box").is(SymbolKind::Struct));
    }

    #[test]
    fn struct_with_conformance_and_where_clause() {
        Test::new(
            r#"module Test
            protocol Equatable { }
            protocol Container[T] { }
            struct Set[T]: Container[T] where T: Equatable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Set").is(SymbolKind::Struct));
    }
}

mod inheritance {
    use super::*;

    #[test]
    fn protocol_inherits_single_protocol() {
        Test::new(
            r#"module Test
            protocol Drawable { }
            protocol Shape: Drawable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Drawable").is(SymbolKind::Protocol))
        .expect(Symbol::new("Shape").is(SymbolKind::Protocol));
    }

    #[test]
    fn protocol_inherits_multiple_protocols() {
        Test::new(
            r#"module Test
            protocol Drawable { }
            protocol Clickable { }
            protocol Widget: Drawable, Clickable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Widget").is(SymbolKind::Protocol));
    }

    #[test]
    fn protocol_with_methods_and_inheritance() {
        Test::new(
            r#"module Test
            protocol Drawable {
                func draw()
            }
            protocol Shape: Drawable {
                func area() -> Int
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Shape").is(SymbolKind::Protocol))
        .expect(Symbol::new("area").is(SymbolKind::Function));
    }

    #[test]
    fn generic_protocol_with_inheritance() {
        Test::new(
            r#"module Test
            protocol Comparable { }
            protocol Sortable[T]: Comparable { }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Sortable").is(SymbolKind::Protocol));
    }
}

mod validation {
    use super::*;

    #[test]
    fn struct_implements_required_method() {
        Test::new(
            r#"module Test
            protocol Drawable {
                func draw()
            }
            struct Circle: Drawable {
                func draw() { }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Circle").is(SymbolKind::Struct));
    }

    #[test]
    fn struct_missing_required_method() {
        Test::new(
            r#"module Test
            protocol Drawable {
                func draw()
            }
            struct Circle: Drawable { }
        "#,
        )
        .expect(HasError("does not implement method 'draw'"));
    }

    #[test]
    fn struct_implements_multiple_methods() {
        Test::new(
            r#"module Test
            protocol Comparable {
                func lessThan(other: Int) -> Bool
                func equals(other: Int) -> Bool
            }
            struct Number: Comparable {
                func lessThan(other: Int) -> Bool { }
                func equals(other: Int) -> Bool { }
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn struct_missing_one_of_multiple_methods() {
        Test::new(
            r#"module Test
            protocol Comparable {
                func lessThan(other: Int) -> Bool
                func equals(other: Int) -> Bool
            }
            struct Number: Comparable {
                func lessThan(other: Int) -> Bool { }
            }
        "#,
        )
        .expect(HasError("does not implement method 'equals'"));
    }

    #[test]
    fn struct_implements_inherited_protocol_methods() {
        Test::new(
            r#"module Test
            protocol Drawable {
                func draw()
            }
            protocol Shape: Drawable {
                func area() -> Int
            }
            struct Circle: Shape {
                func draw() { }
                func area() -> Int { }
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn struct_missing_inherited_protocol_method() {
        Test::new(
            r#"module Test
            protocol Drawable {
                func draw()
            }
            protocol Shape: Drawable {
                func area() -> Int
            }
            struct Circle: Shape {
                func area() -> Int { }
            }
        "#,
        )
        .expect(HasError("does not implement method 'draw'"));
    }

    #[test]
    fn struct_with_method_wrong_return_type() {
        Test::new(
            r#"module Test
            protocol Hashable {
                func hash() -> Int
            }
            struct Point: Hashable {
                func hash() -> String { }
            }
        "#,
        )
        .expect(HasError("method 'hash' has wrong return type"));
    }

    #[test]
    fn struct_with_method_wrong_parameter_count() {
        Test::new(
            r#"module Test
            protocol Comparable {
                func compare(other: Int) -> Bool
            }
            struct Number: Comparable {
                func compare() -> Bool { }
            }
        "#,
        )
        .expect(HasError("does not implement method 'compare'"));
    }

    #[test]
    fn struct_implements_from_multiple_conformances() {
        Test::new(
            r#"module Test
            protocol Drawable {
                func draw()
            }
            protocol Clickable {
                func onClick()
            }
            struct Button: Drawable, Clickable {
                func draw() { }
                func onClick() { }
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn struct_missing_method_from_second_conformance() {
        Test::new(
            r#"module Test
            protocol Drawable {
                func draw()
            }
            protocol Clickable {
                func onClick()
            }
            struct Button: Drawable, Clickable {
                func draw() { }
            }
        "#,
        )
        .expect(HasError("does not implement method 'onClick'"));
    }

    #[test]
    fn empty_protocol_requires_no_methods() {
        Test::new(
            r#"module Test
            protocol Marker { }
            struct Point: Marker { }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn struct_with_labeled_parameter_method() {
        Test::new(
            r#"module Test
            protocol Greetable {
                func greet(with name: String)
            }
            struct Person: Greetable {
                func greet(with name: String) { }
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn struct_with_wrong_label_on_method() {
        Test::new(
            r#"module Test
            protocol Greetable {
                func greet(with name: String)
            }
            struct Person: Greetable {
                func greet(using name: String) { }
            }
        "#,
        )
        .expect(HasError("does not implement method 'greet'"));
    }
}
