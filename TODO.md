# Kestrel TODO

This file tracks immediate next steps for Phase 6.

## Current Priority: Phase 6 - Generics & Protocols

---

## Phase 6: Generics & Protocols

### Generic Constraint Enforcement
**Status**: TODO

Use `where` clause constraints to enable method calls on type parameters.

**Tasks**:
- [ ] Add `get_where_clause(symbol)` helper function
- [ ] Modify `get_type_container()` to handle `TypeParameter` by looking up protocol bounds
- [ ] Collect methods from ALL protocol bounds (not just first)
- [ ] Substitute `Self` with receiver type when looking up protocol methods
- [ ] Handle ambiguous methods (same signature in multiple protocols) with diagnostic
- [ ] Search inherited protocol methods (protocol inheritance chain)
- [ ] Add call-site constraint verification (self-contained, movable function)
- [ ] Emit diagnostic (not hard error) for unsupported generic protocol bounds (defer to associated types)

**New Diagnostics**:
- [ ] `UnconstrainedTypeParameterMemberError` - accessing member on type param with no bounds
- [ ] `MethodNotInBoundsError` - method not found in any protocol bound
- [ ] `AmbiguousConstrainedMethodError` - method found in multiple bounds with same signature
- [ ] `ConstraintNotSatisfiedError` - call site fails to satisfy bound

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

## Future Work (Deferred)

### Static Methods on Type Parameters
**Status**: DEFERRED (implement after constraint enforcement)

Support calling static methods on type parameters: `T.create()`.

**Tasks**:
- [ ] Recognize when a path refers to a type parameter used as a value
- [ ] Handle `TypeParameter.method()` syntax in member resolution
- [ ] Look up static methods from protocol bounds

**Example**:
```kestrel
protocol Factory {
    static func create() -> Self
}

func makeOne[T]() -> T where T: Factory {
    return T.create()  // Static call on type parameter
}
```

### Tighten Type Parameter Assignability
**Status**: DEFERRED (requires proper generic instantiation tracking)

Currently `is_assignable_to` allows any type parameter to be assigned to any other. This is intentionally permissive for Phase 5 but should be tightened.

**Tasks**:
- [ ] Only same type parameter should be assignable to itself
- [ ] Track type parameter identity through function calls
- [ ] Handle generic instantiation properly

---

## Notes

- Type aliases are expanded for comparison
- No implicit coercions (`Int` ≠ `Float`)
- `Self` type is compatible with the containing struct/protocol type
- Type parameter types currently treated as compatible with anything (to be fixed by constraint enforcement)
