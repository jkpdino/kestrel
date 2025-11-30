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

// Note: Protocol inheritance (protocol A: B { }) is not yet implemented in the parser
// These tests should be added once that feature is complete
