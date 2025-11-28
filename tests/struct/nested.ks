// Nested struct declarations

// Basic nested struct
struct Outer {
    struct Inner { }
}

// Multiple levels of nesting
struct Level1 {
    struct Level2 {
        struct Level3 { }
    }
}

// Multiple nested structs in one parent
struct Container {
    struct FirstNested { }
    struct SecondNested { }
    struct ThirdNested { }
}

// Nested structs with visibility modifiers
public struct PublicOuter {
    private struct PrivateInner { }
    public struct PublicInner { }
    internal struct InternalInner { }
}

// Deep nesting
struct A {
    struct B {
        struct C {
            struct D {
                struct E { }
            }
        }
    }
}

// Mixed nested and non-nested structs
struct First { }

struct Second {
    struct NestedInSecond { }
}

struct Third { }

struct Fourth {
    struct NestedOne { }
    struct NestedTwo {
        struct DeeplyNested { }
    }
}
