# Kestrel TODO

This file tracks immediate next steps for Phase 6.

## Current Priority: Phase 6 - Generics & Protocols

---

## Phase 6: Generics & Protocols

### Generic Constraint Enforcement
**Status**: TODO

Use `where` clause constraints to enable method calls on type parameters.

**Tasks**:
- [ ] Look up constraints on type parameters when resolving member access
- [ ] `T: Add` allows calling `a.add(b)` on values of type `T`
- [ ] Verify bounds at call sites (concrete type must satisfy constraints)
- [ ] Constraint satisfaction checking

**Example** (should work after implementation):
```kestrel
protocol Add {
    func add(other: Self) -> Self
}

func addThem[T](a: T, b: T) -> T where T: Add {
    return a.add(b)  // Currently errors: "type 'T' does not have accessible members"
}
```

### Associated Types
**Status**: TODO

Protocol-level type placeholders that conforming types must specify.

**Tasks**:
- [ ] Parser support for `type Item` declarations in protocols
- [ ] Associated type symbol representation
- [ ] Associated type resolution in conforming types
- [ ] Associated type constraints (`where T.Item: Equatable`)

**Example**:
```kestrel
protocol Iterator {
    type Item
    func next() -> Item?
}

struct IntRange: Iterator {
    type Item = Int
    func next() -> Int? { ... }
}
```

### Protocol Method Linking
**Status**: TODO

Link struct methods to the protocol methods they implement.

**Tasks**:
- [ ] Track which protocol a method satisfies when struct conforms
- [ ] Resolve protocol method calls to concrete implementations
- [ ] Error if conforming type is missing required methods

**Example**:
```kestrel
protocol Drawable {
    func draw()
}

struct Circle: Drawable {
    func draw() { ... }  // Linked to Drawable.draw
}

func render[T](item: T) where T: Drawable {
    item.draw()  // Resolves to Circle.draw when T = Circle
}
```

### Extensions with Conformances
**Status**: TODO

Add protocol conformances to existing types via extensions.

**Tasks**:
- [ ] Parser support for `extend Type: Protocol { ... }`
- [ ] Extension symbol representation
- [ ] Methods in extension satisfy protocol requirements
- [ ] Retroactive conformance (add conformance to types you don't own)

**Example**:
```kestrel
protocol Printable {
    func toString() -> String
}

struct Point {
    var x: Int
    var y: Int
}

extend Point: Printable {
    func toString() -> String {
        return "Point"
    }
}
```

---

## Notes

- Type aliases are expanded for comparison
- No implicit coercions (`Int` ≠ `Float`)
- `Self` type is compatible with the containing struct/protocol type
- Type parameter types currently treated as compatible with anything (to be fixed by constraint enforcement)
