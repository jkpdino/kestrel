// Struct declarations with fields

// Struct with a simple field
struct Point {
    let x: Int
    let y: Int
}

// Struct with mutable fields
struct MutablePoint {
    var x: Float
    var y: Float
}

// Struct with mixed immutable and mutable fields
struct Player {
    let id: Int
    let name: String
    var score: Int
    var health: Int
}

// Struct with static fields
struct Config {
    static let version: String
    static var instanceCount: Int
}

// Struct with visibility modifiers on fields
struct Person {
    public let name: String
    private var age: Int
    internal let email: String
    fileprivate var address: String
}

// Struct with all modifiers combined
struct Singleton {
    public static let shared: Singleton
    private static var count: Int
    public let id: String
    private var data: String
}

// Nested struct with fields
struct Outer {
    let value: Int

    struct Inner {
        let innerValue: String
        var mutableInner: Bool
    }
}

// Struct with path types
struct Container {
    let item: Some.Nested.Type
    var list: Collections.List
}

// Empty struct (for comparison)
struct Empty { }

// Struct with single field
struct Wrapper {
    let wrapped: Any
}
