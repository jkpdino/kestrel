// Edge cases for struct declarations

// Struct with only static fields
struct StaticOnly {
    static let constant: Int
    static var shared: String
}

// Struct with deeply nested inner structs containing fields
struct DeepNesting {
    let outerField: Int

    struct Level1 {
        let level1Field: String

        struct Level2 {
            let level2Field: Bool
            var mutableLevel2: Float
        }
    }
}

// Multiple structs with same field names (different scopes)
struct First {
    let value: Int
    let name: String
}

struct Second {
    let value: Float
    let name: String
}

// Struct with various type forms
struct TypeVariety {
    let simple: Int
    let qualified: Some.Type
    let deepPath: A.B.C.D
}

// All visibility modifiers on struct itself
public struct PublicStruct {
    let field: Int
}

private struct PrivateStruct {
    let field: Int
}

internal struct InternalStruct {
    let field: Int
}

fileprivate struct FileprivateStruct {
    let field: Int
}

// Struct mixing nested structs, modules, and imports
struct ComplexContainer {
    let containerField: Int

    struct NestedOne {
        let nestedField: String
    }

    struct NestedTwo {
        var anotherField: Bool
    }
}

// Single character names
struct A {
    let b: C
}

// Longer descriptive names
struct VeryLongStructNameForTesting {
    let veryLongFieldNameForTestingPurposes: SomeExtremelyLongTypeName
}
