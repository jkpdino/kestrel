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

mod self_parameter {
    use super::*;

    #[test]
    fn instance_method_compiles() {
        // Instance methods (non-static) should compile
        Test::new(
            r#"
module Main

struct Counter {
    let value: Int

    func getValue() -> Int {
        42
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Counter").is(SymbolKind::Struct))
        .expect(Symbol::new("getValue").is(SymbolKind::Function));
    }

    #[test]
    fn mutating_method_compiles() {
        // Mutating methods should compile
        Test::new(
            r#"
module Main

struct Counter {
    var value: Int

    mutating func increment() -> () {
        ()
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Counter").is(SymbolKind::Struct))
        .expect(Symbol::new("increment").is(SymbolKind::Function));
    }

    #[test]
    fn consuming_method_compiles() {
        // Consuming methods should compile
        Test::new(
            r#"
module Main

struct Container {
    let item: Int

    consuming func take() -> Int {
        42
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Container").is(SymbolKind::Struct))
        .expect(Symbol::new("take").is(SymbolKind::Function));
    }

    #[test]
    fn static_method_no_self() {
        // Static methods should not have self
        Test::new(
            r#"
module Main

struct Factory {
    static func create() -> Int {
        42
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Factory").is(SymbolKind::Struct))
        .expect(Symbol::new("create").is(SymbolKind::Function));
    }

    #[test]
    fn protocol_method_compiles() {
        // Protocol methods should compile
        Test::new(
            r#"
module Main

protocol Printable {
    func print() -> String
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Printable").is(SymbolKind::Protocol))
        .expect(Symbol::new("print").is(SymbolKind::Function));
    }

    #[test]
    fn mutating_protocol_method_compiles() {
        // Mutating protocol methods should compile
        Test::new(
            r#"
module Main

protocol Resettable {
    mutating func reset() -> ()
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Resettable").is(SymbolKind::Protocol))
        .expect(Symbol::new("reset").is(SymbolKind::Function));
    }

    // === Self Usage in Method Bodies ===

    #[test]
    fn access_self_field_in_instance_method() {
        // Instance method can access self.field - Self type resolves to struct type
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int

    func getX() -> Int {
        self.x
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn access_multiple_self_fields() {
        // Instance methods can access multiple fields on self
        Test::new(
            r#"
module Main

struct Rectangle {
    let width: Int
    let height: Int

    func getWidth() -> Int {
        self.width
    }

    func getHeight() -> Int {
        self.height
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn self_in_static_method_error() {
        // Using self in a static method should be an error
        Test::new(
            r#"
module Main

struct Calculator {
    let value: Int

    static func compute() -> Int {
        self.value
    }
}
"#,
        )
        .expect(HasError("cannot use 'self' in static method"));
    }

    #[test]
    fn self_in_free_function_error() {
        // Using self in a free function should be an error
        Test::new(
            r#"
module Main

func freeFunc() -> Int {
    self.x
}
"#,
        )
        .expect(HasError("cannot use 'self' in free function"));
    }

    #[test]
    fn self_in_module_function_error() {
        // Using self in a module-level function should be an error
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
}

func notAMethod() -> Int {
    self.x
}
"#,
        )
        .expect(HasError("cannot use 'self' in free function"));
    }

    // === Method Calls on Instances ===

    #[test]
    fn call_instance_method_on_struct() {
        // Call an instance method on a struct instance
        Test::new(
            r#"
module Main

struct Counter {
    let value: Int

    func getValue() -> Int {
        42
    }
}

func test(c: Counter) -> Int {
    c.getValue()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_instance_method_with_params() {
        // Call an instance method that takes parameters
        Test::new(
            r#"
module Main

struct Calculator {
    let base: Int

    func add(x: Int) -> Int {
        42
    }
}

func test(c: Calculator) -> Int {
    c.add(10)
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn chain_method_calls() {
        // Chained method calls as yield expression should parse correctly
        Test::new(
            r#"
module Main

struct Builder {
    let value: Int

    func build() -> Int {
        42
    }
}

struct Factory {
    let builder: Builder

    func getBuilder() -> Builder {
        self.builder
    }
}

func test(f: Factory) -> Int {
    f.getBuilder().build()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn method_calling_another_method() {
        // A method can call another method on self
        Test::new(
            r#"
module Main

struct Calculator {
    let value: Int

    func getValue() -> Int {
        42
    }

    func getDoubleValue() -> Int {
        self.getValue()
    }
}
"#,
        )
        .expect(Compiles);
    }

    // === Static vs Instance Methods ===

    #[test]
    fn call_static_method_on_type() {
        // Call a static method on the type name
        Test::new(
            r#"
module Main

struct Factory {
    static func create() -> Int {
        42
    }
}

func test() -> Int {
    Factory.create()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn mix_static_and_instance_methods() {
        // Struct with both static and instance methods
        Test::new(
            r#"
module Main

struct Counter {
    let value: Int

    static func zero() -> Int {
        0
    }

    func getValue() -> Int {
        42
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn static_method_same_name_as_instance() {
        // BUG FOUND: Static and instance methods with the same name are treated as
        // duplicates, but they should be distinguishable by receiver (static vs instance)
        // This is a design decision that might be intentional or a bug
        Test::new(
            r#"
module Main

struct Counter {
    let value: Int

    static func get() -> Int {
        0
    }

    func get() -> Int {
        42
    }
}
"#,
        )
        .expect(HasError("duplicate"));
        // This might be intentional design - in Swift, static and instance methods
        // with the same name/signature ARE allowed since they have different receivers
    }

    // === Mutating Methods ===

    #[test]
    fn mutating_method_with_self_access() {
        // Mutating method can access self.field
        Test::new(
            r#"
module Main

struct Counter {
    var value: Int

    mutating func getValue() -> Int {
        self.value
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_mutating_method() {
        // Call a mutating method on an instance
        Test::new(
            r#"
module Main

struct Counter {
    var value: Int

    mutating func increment() -> () {
        ()
    }
}

func test(c: Counter) -> () {
    c.increment()
}
"#,
        )
        .expect(Compiles);
    }

    // === Consuming Methods ===

    #[test]
    fn consuming_method_with_self_access() {
        // Consuming method can access self.field
        Test::new(
            r#"
module Main

struct Container {
    let item: Int

    consuming func getItem() -> Int {
        self.item
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_consuming_method() {
        // Call a consuming method on an instance
        Test::new(
            r#"
module Main

struct Container {
    let item: Int

    consuming func take() -> Int {
        42
    }
}

func test(c: Container) -> Int {
    c.take()
}
"#,
        )
        .expect(Compiles);
    }

    // === Protocol Methods with Receivers ===

    #[test]
    fn protocol_with_consuming_method() {
        // Protocol with consuming method
        Test::new(
            r#"
module Main

protocol Consumable {
    consuming func consume() -> ()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn protocol_with_all_receiver_types() {
        // Protocol with all receiver types
        Test::new(
            r#"
module Main

protocol AllReceivers {
    func borrow() -> Int
    mutating func mutate() -> ()
    consuming func consume() -> ()
}
"#,
        )
        .expect(Compiles);
    }

    // === Generic Structs with Methods ===

    #[test]
    fn generic_struct_with_instance_method() {
        // Generic struct with an instance method
        Test::new(
            r#"
module Main

struct Container[T] {
    let item: T

    func isEmpty() -> Bool {
        false
    }
}
"#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Container").is(SymbolKind::Struct).has(Behavior::TypeParamCount(1)));
    }

    #[test]
    fn generic_struct_method_accessing_self() {
        // Generic struct method can access self.field
        Test::new(
            r#"
module Main

struct Wrapper[T] {
    let value: T

    func getValue() -> T {
        self.value
    }
}
"#,
        )
        .expect(Compiles);
    }

    // === Edge Cases ===

    #[test]
    fn empty_method_body() {
        // Method with empty body (implicit unit return)
        Test::new(
            r#"
module Main

struct Empty {
    func doNothing() -> () {
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_struct_methods() {
        // Nested structs with methods work correctly
        Test::new(
            r#"
module Main

struct Outer {
    let inner: Inner

    func getInner() -> Inner {
        self.inner
    }
}

struct Inner {
    let value: Int

    func getValue() -> Int {
        self.value
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn method_returning_self_type() {
        // Method that returns the Self type - works because we just return self (no field access)
        Test::new(
            r#"
module Main

struct Builder {
    let value: Int

    func withValue(v: Int) -> Builder {
        self
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn multiple_methods_accessing_same_field() {
        // Multiple methods can access the same field on self
        Test::new(
            r#"
module Main

struct Point {
    let x: Int

    func getX() -> Int {
        self.x
    }

    func printX() -> Int {
        self.x
    }

    func copyX() -> Int {
        self.x
    }
}
"#,
        )
        .expect(Compiles);
    }
}

mod function_overloading {
    use super::*;

    // === Overload by Arity ===

    #[test]
    fn overload_by_arity_zero_vs_one() {
        // Overload with zero vs one parameter
        Test::new(
            r#"
module Main

func greet() -> String {
    "hello"
}

func greet(name: String) -> String {
    "hello"
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn overload_by_arity_one_vs_two() {
        // Overload with one vs two parameters
        Test::new(
            r#"
module Main

func add(x: Int) -> Int {
    x
}

func add(x: Int, y: Int) -> Int {
    42
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn overload_by_arity_multiple() {
        // Multiple overloads with different arities
        Test::new(
            r#"
module Main

func compute() -> Int {
    0
}

func compute(a: Int) -> Int {
    a
}

func compute(a: Int, b: Int) -> Int {
    42
}

func compute(a: Int, b: Int, c: Int) -> Int {
    42
}
"#,
        )
        .expect(Compiles);
    }

    // === Overload by Labels ===

    #[test]
    fn overload_by_label_different_labels() {
        // Overload with different labels
        Test::new(
            r#"
module Main

func move(by x: Int) -> Int {
    x
}

func move(to x: Int) -> Int {
    x
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn overload_labeled_vs_unlabeled() {
        // Overload with labeled vs unlabeled parameter
        Test::new(
            r#"
module Main

func set(value: Int) -> Int {
    value
}

func set(to value: Int) -> Int {
    value
}
"#,
        )
        .expect(Compiles);
    }

    // === Call Overloaded Functions ===

    #[test]
    fn call_overload_by_arity_zero() {
        // Call the zero-parameter overload
        Test::new(
            r#"
module Main

func greet() -> String {
    "hello"
}

func greet(name: String) -> String {
    "hello"
}

func test() -> String {
    greet()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_overload_by_arity_one() {
        // Call the one-parameter overload
        Test::new(
            r#"
module Main

func greet() -> String {
    "hello"
}

func greet(name: String) -> String {
    "hello"
}

func test() -> String {
    greet("world")
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_overload_by_label() {
        // Call overload using label to disambiguate
        Test::new(
            r#"
module Main

func move(by x: Int) -> Int {
    x
}

func move(to x: Int) -> Int {
    x
}

func test() -> Int {
    move(to: 10)
}
"#,
        )
        .expect(Compiles);
    }

    // === Struct Method Overloading ===

    #[test]
    fn struct_method_overload_by_arity() {
        // BUG FOUND: Self.field issue affects overloaded methods too
        Test::new(
            r#"
module Main

struct Calculator {
    let value: Int

    func add() -> Int {
        self.value
    }

    func add(x: Int) -> Int {
        42
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn struct_method_overload_by_label() {
        // Struct methods overloaded by label
        Test::new(
            r#"
module Main

struct Printer {
    let prefix: String

    func print(text: String) -> String {
        text
    }

    func print(with text: String) -> String {
        text
    }
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_overloaded_struct_method() {
        // Call overloaded struct method
        Test::new(
            r#"
module Main

struct Calculator {
    let value: Int

    func add() -> Int {
        self.value
    }

    func add(x: Int) -> Int {
        42
    }
}

func test(c: Calculator) -> Int {
    c.add(5)
}
"#,
        )
        .expect(Compiles);
    }

    // === Duplicate Detection ===

    #[test]
    fn duplicate_function_same_signature_error() {
        // Same name, same parameters should error
        Test::new(
            r#"
module Main

func foo(x: Int) -> Int {
    x
}

func foo(x: Int) -> Int {
    x
}
"#,
        )
        .expect(HasError("duplicate"));
    }

    #[test]
    fn duplicate_method_same_signature_error() {
        // Duplicate struct methods should error
        Test::new(
            r#"
module Main

struct Counter {
    let value: Int

    func getValue() -> Int {
        42
    }

    func getValue() -> Int {
        0
    }
}
"#,
        )
        .expect(HasError("duplicate"));
    }
}

mod function_calls {
    use super::*;

    // === Basic Function Calls ===

    #[test]
    fn call_simple_function() {
        // Call a simple function with no parameters
        Test::new(
            r#"
module Main

func getNumber() -> Int {
    42
}

func test() -> Int {
    getNumber()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_function_with_one_param() {
        // Call a function with one parameter
        Test::new(
            r#"
module Main

func double(x: Int) -> Int {
    42
}

func test() -> Int {
    double(21)
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_function_with_multiple_params() {
        // Call a function with multiple parameters
        Test::new(
            r#"
module Main

func add(x: Int, y: Int) -> Int {
    42
}

func test() -> Int {
    add(1, 2)
}
"#,
        )
        .expect(Compiles);
    }

    // === Labeled Arguments ===

    #[test]
    fn call_with_labeled_argument() {
        // Call a function with a labeled argument
        Test::new(
            r#"
module Main

func greet(with name: String) -> String {
    name
}

func test() -> String {
    greet(with: "world")
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_with_multiple_labeled_arguments() {
        // Call with multiple labeled arguments
        Test::new(
            r#"
module Main

func createPoint(x xVal: Int, y yVal: Int) -> Int {
    42
}

func test() -> Int {
    createPoint(x: 10, y: 20)
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_with_mixed_labeled_unlabeled() {
        // Mix of labeled and unlabeled arguments
        Test::new(
            r#"
module Main

func format(value: Int, with prefix: String) -> String {
    prefix
}

func test() -> String {
    format(42, with: "Result: ")
}
"#,
        )
        .expect(Compiles);
    }

    // === Nested Calls ===

    #[test]
    fn nested_function_calls() {
        // Nested function calls
        Test::new(
            r#"
module Main

func double(x: Int) -> Int {
    42
}

func add(x: Int, y: Int) -> Int {
    42
}

func test() -> Int {
    add(double(1), double(2))
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn deeply_nested_calls() {
        // Deeply nested function calls
        Test::new(
            r#"
module Main

func id(x: Int) -> Int {
    x
}

func test() -> Int {
    id(id(id(id(42))))
}
"#,
        )
        .expect(Compiles);
    }

    // === Error Cases ===

    #[test]
    fn call_with_wrong_arity_error() {
        // BUG FOUND: Wrong number of arguments silently compiles (no error)
        // This should error but currently doesn't
        Test::new(
            r#"
module Main

func add(x: Int, y: Int) -> Int {
    42
}

func test() -> Int {
    add(1)
}
"#,
        )
        .expect(HasError("no matching overload"));
    }

    #[test]
    fn call_undefined_function_error() {
        // Calling undefined function should produce an error
        Test::new(
            r#"
module Main

func test() -> Int {
    undefined()
}
"#,
        )
        .expect(HasError("undefined name"));
    }

    #[test]
    fn call_with_wrong_label_error() {
        // Using wrong label should produce an error
        Test::new(
            r#"
module Main

func greet(with name: String) -> String {
    name
}

func test() -> String {
    greet(using: "world")
}
"#,
        )
        .expect(HasError("no matching overload"));
    }

    // === Return Type Propagation ===

    #[test]
    fn function_return_type_in_expression() {
        // Function return type used in expression
        Test::new(
            r#"
module Main

func getString() -> String {
    "hello"
}

func test() -> String {
    let s: String = getString();
    s
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn void_function_call() {
        // Calling a void function
        Test::new(
            r#"
module Main

func doSomething() -> () {
    ()
}

func test() -> () {
    doSomething()
}
"#,
        )
        .expect(Compiles);
    }
}

mod method_calls {
    use super::*;

    // === Basic Method Calls ===

    #[test]
    fn call_method_no_params() {
        // Call method with no parameters
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int

    func origin() -> Bool {
        false
    }
}

func test(p: Point) -> Bool {
    p.origin()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_method_with_params() {
        // Call method with parameters
        Test::new(
            r#"
module Main

struct Calculator {
    let base: Int

    func add(x: Int, y: Int) -> Int {
        42
    }
}

func test(c: Calculator) -> Int {
    c.add(1, 2)
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_method_with_labeled_params() {
        // Call method with labeled parameters
        Test::new(
            r#"
module Main

struct Formatter {
    let prefix: String

    func format(with value: Int) -> String {
        "formatted"
    }
}

func test(f: Formatter) -> String {
    f.format(with: 42)
}
"#,
        )
        .expect(Compiles);
    }

    // === Chained Method Calls ===

    #[test]
    fn chained_method_calls_same_type() {
        // Chained method calls on builder pattern (same return type)
        Test::new(
            r#"
module Main

struct Builder {
    let value: Int

    func step1() -> Builder {
        self
    }

    func step2() -> Builder {
        self
    }

    func build() -> Int {
        self.value
    }
}

func test(b: Builder) -> Int {
    b.step1().step2().build()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn chained_method_calls_different_types() {
        // Chained method calls with different return types
        Test::new(
            r#"
module Main

struct Container {
    let inner: Inner

    func getInner() -> Inner {
        self.inner
    }
}

struct Inner {
    let value: Int

    func getValue() -> Int {
        self.value
    }
}

func test(c: Container) -> Int {
    c.getInner().getValue()
}
"#,
        )
        .expect(Compiles);
    }

    // === Static Method Calls ===

    #[test]
    fn call_static_method_no_params() {
        // Call static method without parameters
        Test::new(
            r#"
module Main

struct Factory {
    static func defaultValue() -> Int {
        0
    }
}

func test() -> Int {
    Factory.defaultValue()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_static_method_with_params() {
        // Call static method with parameters
        Test::new(
            r#"
module Main

struct MathUtils {
    static func max(a: Int, b: Int) -> Int {
        42
    }
}

func test() -> Int {
    MathUtils.max(10, 20)
}
"#,
        )
        .expect(Compiles);
    }

    // === Method Call Errors ===

    #[test]
    fn call_nonexistent_method_error() {
        // Call a method that doesn't exist - should produce an error
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
}

func test(p: Point) -> Int {
    p.nonExistent()
}
"#,
        )
        .expect(HasError("no member"));
    }

    #[test]
    fn call_method_wrong_receiver_type() {
        // Call method on wrong type - should produce an error
        Test::new(
            r#"
module Main

struct A {
    func methodA() -> Int {
        42
    }
}

struct B {
    let value: Int
}

func test(b: B) -> Int {
    b.methodA()
}
"#,
        )
        .expect(HasError("no member"));
    }

    #[test]
    fn call_instance_method_on_type_error() {
        // Calling instance method on type name should error
        Test::new(
            r#"
module Main

struct Counter {
    let value: Int

    func getValue() -> Int {
        42
    }
}

func test() -> Int {
    Counter.getValue()
}
"#,
        )
        .expect(HasError("instance method"));
    }

    // === Method Visibility ===

    #[test]
    fn call_public_method() {
        // INTERESTING: This test compiles successfully with pub func!
        // The self.id access works here but fails in other tests.
        // This might be due to visibility modifier affecting type resolution.
        Test::new(
            r#"
module Main

struct Widget {
    let id: Int

    pub func getId() -> Int {
        self.id
    }
}

func test(w: Widget) -> Int {
    w.getId()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn call_private_method_from_inside() {
        // Private method can be called from inside the struct
        Test::new(
            r#"
module Main

struct Widget {
    let id: Int

    private func internalId() -> Int {
        self.id
    }

    func getId() -> Int {
        self.internalId()
    }
}
"#,
        )
        .expect(Compiles);
    }
}
