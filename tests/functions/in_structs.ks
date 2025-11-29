module Functions.InStructs

// ============================================================
// FUNCTIONS INSIDE STRUCTS (Methods)
// ============================================================

struct Point {
    let x: Int
    let y: Int

    // Basic method
    func distance() -> Float { }

    // Method with parameters
    func add(other: Point) -> Point { }

    // Static method
    static func origin() -> Point { }

    // Public method
    public func publicMethod() { }

    // Private method
    private func privateMethod() { }

    // Valid overloads within struct
    func overloaded(x: Int) { }
    func overloaded(x: Float) { }
    func overloaded(x: Int, y: Int) { }

    // Labeled method
    func move(by offset: Point) { }
    func move(to destination: Point) { }
}

struct Calculator {
    // Methods with same name as Point - should be fine (different scope)
    func add(x: Int, y: Int) -> Int { }
    func distance(from a: Int, to b: Int) -> Int { }

    // Duplicate within struct (should error)
    func duplicate(x: Int) { }
    func duplicate(y: Int) { }
}

// Nested struct with functions
struct Outer {
    let value: Int

    func outerMethod() { }

    struct Inner {
        let innerValue: Int

        func innerMethod() { }

        // Same name as outer - should be fine (different scope)
        func outerMethod() { }
    }
}

// Empty struct with only methods
struct MethodOnly {
    func doSomething() { }
    static func create() -> MethodOnly { }
}
