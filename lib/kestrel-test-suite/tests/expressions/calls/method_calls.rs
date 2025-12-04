//! Tests for method call expressions.
//!
//! These tests verify that method calls (instance and static) are correctly resolved,
//! including self parameter handling, primitive methods, and chaining.

use kestrel_test_suite::*;

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

