//! Tests for variable declarations (let/var).
//!
//! These tests verify that local variable declarations are correctly resolved
//! in function bodies.

use kestrel_test_suite::*;

mod local_variables {
    use super::*;

    #[test]
    fn let_declaration_with_type_and_initializer() {
        Test::new("module Main\nfunc test() -> Int { let x: Int = 42; x }")
            .expect(Compiles);
    }

    #[test]
    fn var_declaration_with_type_and_initializer() {
        Test::new("module Main\nfunc test() -> Int { var x: Int = 42; x }")
            .expect(Compiles);
    }

    #[test]
    fn let_declaration_with_type_only() {
        // Type annotation without initializer (requires type)
        Test::new("module Main\nfunc test() -> Int { let x: Int = 0; x }")
            .expect(Compiles);
    }

    #[test]
    fn multiple_let_declarations() {
        Test::new("module Main\nfunc test() -> Int { let x: Int = 1; let y: Int = 2; y }")
            .expect(Compiles);
    }

    #[test]
    fn let_then_reference() {
        // Declare a variable, then return it
        Test::new("module Main\nfunc test() -> String { let msg: String = \"hello\"; msg }")
            .expect(Compiles);
    }

    #[test]
    fn shadowing_parameter() {
        // Local variable shadows parameter
        Test::new("module Main\nfunc test(x: Int) -> Int { let x: Int = 99; x }")
            .expect(Compiles);
    }

    #[test]
    fn array_variable() {
        Test::new("module Main\nfunc test() -> [Int] { let arr: [Int] = [1, 2, 3]; arr }")
            .expect(Compiles);
    }

    #[test]
    fn tuple_variable() {
        Test::new("module Main\nfunc test() -> (Int, String) { let pair: (Int, String) = (42, \"hi\"); pair }")
            .expect(Compiles);
    }
}

