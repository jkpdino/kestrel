module Functions.InStructs

// ============================================================
// FUNCTIONS INSIDE STRUCTS (Methods)
// ============================================================

struct Point {
    let x: Int
    let y: Int

    // Basic method
    fn distance() -> Float { }

    // Method with parameters
    fn add(other: Point) -> Point { }

    // Static method
    static fn origin() -> Point { }

    // Public method
    public fn publicMethod() { }

    // Private method
    private fn privateMethod() { }

    // Valid overloads within struct
    fn overloaded(x: Int) { }
    fn overloaded(x: Float) { }
    fn overloaded(x: Int, y: Int) { }

    // Labeled method
    fn move(by offset: Point) { }
    fn move(to destination: Point) { }
}

struct Calculator {
    // Methods with same name as Point - should be fine (different scope)
    fn add(x: Int, y: Int) -> Int { }
    fn distance(from a: Int, to b: Int) -> Int { }

    // Duplicate within struct (should error)
    fn duplicate(x: Int) { }
    fn duplicate(y: Int) { }
}

// Nested struct with functions
struct Outer {
    let value: Int

    fn outerMethod() { }

    struct Inner {
        let innerValue: Int

        fn innerMethod() { }

        // Same name as outer - should be fine (different scope)
        fn outerMethod() { }
    }
}

// Empty struct with only methods
struct MethodOnly {
    fn doSomething() { }
    static fn create() -> MethodOnly { }
}
