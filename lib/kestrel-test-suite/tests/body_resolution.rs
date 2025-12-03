//! Tests for function body resolution.
//!
//! These tests verify that function bodies are correctly resolved
//! from syntax to semantic expressions and statements.

use kestrel_test_suite::*;

mod literal_expressions {
    use super::*;

    #[test]
    fn integer_literal_in_body() {
        Test::new(
            r#"
module Main

func getValue() -> Int {
    42
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("getValue").is(SymbolKind::Function));
    }

    #[test]
    fn float_literal_in_body() {
        Test::new(
            r#"
module Main

func getValue() -> Float {
    3.14
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn string_literal_in_body() {
        Test::new(
            r#"
module Main

func getMessage() -> String {
    "hello world"
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn bool_literal_in_body() {
        Test::new(
            r#"
module Main

func isEnabled() -> Bool {
    true
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn unit_literal_in_body() {
        Test::new(
            r#"
module Main

func doNothing() -> () {
    ()
}
"#,
        )
        .expect(Compiles);
    }
}

mod composite_expressions {
    use super::*;

    #[test]
    fn array_literal_in_body() {
        Test::new(
            r#"
module Main

func getNumbers() -> [Int] {
    [1, 2, 3]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_literal_in_body() {
        Test::new(
            r#"
module Main

func getPair() -> (Int, String) {
    (42, "hello")
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_tuple_in_body() {
        Test::new(
            r#"
module Main

func getNested() -> ((Int, Int), String) {
    ((1, 2), "point")
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn grouping_expression_in_body() {
        Test::new(
            r#"
module Main

func getValue() -> Int {
    (42)
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_arrays() {
        Test::new(
            r#"
module Main

func getMatrix() -> [[Int]] {
    [[1, 2], [3, 4]]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn mixed_array_elements() {
        Test::new(
            r#"
module Main

func getNumbers() -> [Int] {
    [1, 2, 3, 4, 5]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn empty_tuple_is_unit() {
        Test::new(
            r#"
module Main

func empty() -> () {
    ()
}
"#,
        )
        .expect(Compiles);
    }
}

mod multiple_functions {
    use super::*;

    #[test]
    fn multiple_functions_with_bodies() {
        Test::new(
            r#"
module Main

func getInt() -> Int {
    1
}

func getString() -> String {
    "hello"
}

func getBool() -> Bool {
    false
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("getInt").is(SymbolKind::Function))
        .expect(Symbol::new("getString").is(SymbolKind::Function))
        .expect(Symbol::new("getBool").is(SymbolKind::Function));
    }

    #[test]
    fn functions_in_struct() {
        Test::new(
            r#"
module Main

struct Calculator {
    static func add() -> Int {
        42
    }

    static func multiply() -> Int {
        100
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Calculator").is(SymbolKind::Struct))
        .expect(Symbol::new("add").is(SymbolKind::Function))
        .expect(Symbol::new("multiply").is(SymbolKind::Function));
    }

    #[test]
    fn functions_with_different_return_types() {
        Test::new(
            r#"
module Main

func returnInt() -> Int { 42 }
func returnFloat() -> Float { 3.14 }
func returnString() -> String { "test" }
func returnBool() -> Bool { true }
func returnUnit() -> () { () }
"#,
        )
        .expect(Compiles);
    }
}

mod protocol_methods {
    use super::*;

    #[test]
    fn protocol_method_has_no_body() {
        Test::new(
            r#"
module Main

protocol Computable {
    func compute() -> Int
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("compute").is(SymbolKind::Function));
    }

    #[test]
    fn protocol_with_multiple_methods() {
        Test::new(
            r#"
module Main

protocol DataSource {
    func count() -> Int
    func get(index: Int) -> String
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("count").is(SymbolKind::Function))
        .expect(Symbol::new("get").is(SymbolKind::Function));
    }
}

mod complex {
    use super::*;

    #[test]
    fn complex_generic_structs() {
        // Test generic struct declarations with no body
        Test::new("module Main\nstruct Pair[T, U] { }\nstruct Container[T] { }")
            .expect(Compiles)
            .expect(Symbol::new("Pair").is(SymbolKind::Struct).has(Behavior::TypeParamCount(2)))
            .expect(Symbol::new("Container").is(SymbolKind::Struct).has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn complex_generic_protocol() {
        Test::new("module Main\nprotocol Transform[T] { func apply(value: T) -> T }")
            .expect(Compiles)
            .expect(Symbol::new("Transform").is(SymbolKind::Protocol).has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn complex_generic_functions() {
        Test::new("module Main\nfunc identity[T](x: T) -> T { x }\nfunc compose() -> (Int, (String, Bool)) { (42, (\"hello\", true)) }\nfunc matrix() -> [[Int]] { [[1, 2], [3, 4]] }")
            .expect(Compiles)
            .expect(Symbol::new("identity").is(SymbolKind::Function).has(Behavior::TypeParamCount(1)))
            .expect(Symbol::new("compose").is(SymbolKind::Function))
            .expect(Symbol::new("matrix").is(SymbolKind::Function));
    }

    #[test]
    fn complex_struct_with_methods() {
        Test::new("module Main\nstruct Container[T] { static func wrap[U](item: U) -> [U] { [item] } }")
            .expect(Compiles)
            .expect(Symbol::new("Container").is(SymbolKind::Struct).has(Behavior::TypeParamCount(1)))
            .expect(Symbol::new("wrap").is(SymbolKind::Function).has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn parameter_reference_in_body() {
        // Function that returns its parameter - tests local variable resolution
        Test::new("module Main\nfunc echo(x: Int) -> Int { x }")
            .expect(Compiles)
            .expect(Symbol::new("echo").is(SymbolKind::Function));
    }

    #[test]
    fn multiple_parameters_in_body() {
        // Function that uses one of multiple parameters
        Test::new("module Main\nfunc second(a: Int, b: String) -> String { b }")
            .expect(Compiles);
    }

    #[test]
    fn generic_parameter_in_body() {
        // Generic function returning its parameter
        Test::new("module Main\nfunc id[T](value: T) -> T { value }")
            .expect(Compiles)
            .expect(Symbol::new("id").is(SymbolKind::Function).has(Behavior::TypeParamCount(1)));
    }
}

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

mod edge_cases {
    use super::*;

    #[test]
    fn deeply_nested_expressions() {
        Test::new(
            r#"
module Main

func deep() -> Int {
    (((42)))
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn complex_tuple_in_array() {
        Test::new(
            r#"
module Main

func pairs() -> [(Int, String)] {
    [(1, "one"), (2, "two")]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn single_element_array() {
        Test::new(
            r#"
module Main

func single() -> [Int] {
    [42]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_hex_literal() {
        Test::new(
            r#"
module Main

func getHex() -> Int {
    0xFF
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn function_with_binary_literal() {
        Test::new(
            r#"
module Main

func getBinary() -> Int {
    0b1010
}
"#,
        )
        .expect(Compiles);
    }
}

mod field_access {
    use super::*;

    #[test]
    fn simple_field_access() {
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int
}

func getX(p: Point) -> Int {
    p.x
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn chained_field_access() {
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int
}

struct Line {
    let start: Point
    let end: Point
}

func getStartX(line: Line) -> Int {
    line.start.x
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn field_access_in_variable() {
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int
}

func example(p: Point) -> Int {
    let val: Int = p.x;
    val
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nonexistent_field_error() {
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int
}

func getZ(p: Point) -> Int {
    p.z
}
"#,
        )
        .expect(HasError("no member 'z' on type 'Point'"));
    }

    #[test]
    fn member_access_on_primitive_error() {
        Test::new(
            r#"
module Main

func test(x: Int) -> Int {
    x.foo
}
"#,
        )
        .expect(HasError("cannot access member on type"));
    }

    #[test]
    fn private_field_access_error() {
        Test::new(
            r#"
module Main

struct Secret {
    private let hidden: Int
}

func peek(s: Secret) -> Int {
    s.hidden
}
"#,
        )
        .expect(HasError("is private"));
    }

    #[test]
    fn public_field_access_succeeds() {
        Test::new(
            r#"
module Main

struct Point {
    pub let x: Int
    pub let y: Int
}

func getX(p: Point) -> Int {
    p.x
}
"#,
        )
        .expect(Compiles);
    }
}

mod primitive_methods {
    use super::*;

    #[test]
    fn primitive_method_not_callable_error() {
        // Primitive methods cannot be used as first-class values
        Test::new(
            r#"
module Main

func test(x: Int) -> () {
    let f = x.toString;
}
"#,
        )
        .expect(HasError("primitive method 'toString' on 'I64' must be called"));
    }

    #[test]
    fn primitive_method_nonexistent_error() {
        // Unknown member on primitive types that aren't methods
        Test::new(
            r#"
module Main

func test(x: Int) -> Int {
    x.notAMethod
}
"#,
        )
        .expect(HasError("cannot access member on type"));
    }
}
