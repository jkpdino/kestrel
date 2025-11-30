# Classes

Classes are reference types that can contain fields, methods, and nested type declarations.

## Syntax

```
ClassDeclaration → Visibility? CLASS Identifier LBRACE ClassMember* RBRACE

ClassMember → FunctionDeclaration
            | FieldDeclaration
            | ClassDeclaration
            | StructDeclaration
            | ProtocolDeclaration
```

### Tokens
- `CLASS` - The `class` keyword
- `LBRACE` / `RBRACE` - Curly braces `{` `}`
- `Visibility` - Optional visibility modifier

## Examples

### Empty Class

```kestrel
class Empty { }
```

### Class with Fields

```kestrel
class Person {
    let name: String
    let age: Int
    var active: Bool
}
```

### Class with Methods

```kestrel
class Calculator {
    func add(a: Int, b: Int) -> Int { }
    func subtract(a: Int, b: Int) -> Int { }

    static func create() -> Calculator { }
}
```

### Class with Mixed Members

```kestrel
class User {
    // Fields
    let id: Int
    var name: String
    private var password: String

    // Instance methods
    func displayName() -> String { }
    func updatePassword(new: String) { }

    // Static methods
    static func create(name: String) -> User { }
}
```

### Nested Types

```kestrel
class Outer {
    class Inner {
        let value: Int
    }

    struct Point {
        let x: Int
        let y: Int
    }

    protocol Handler {
        func handle()
    }
}
```

### Class with Visibility

```kestrel
public class PublicAPI {
    public let id: Int
    internal var state: String
    private var cache: Data

    public func fetch() -> Data { }
    private func loadFromCache() -> Data { }
}
```

## Semantic Rules

### Rule 1: No Duplicate Type Names

Within a class, nested type names must be unique.

```
ERROR: DuplicateSymbolPass error
WHEN: Two types with the same name in the same class
WHY: Ambiguous type reference
```

**Example (invalid):**
```kestrel
class Container {
    class Item { }
    struct Item { }    // ERROR: duplicate type 'Item'
}
```

### Rule 2: No Duplicate Member Names (Non-Functions)

Fields and non-overloaded declarations must have unique names.

```
ERROR: DuplicateSymbolPass error
WHEN: Two fields with the same name, or field and function with same name
WHY: Ambiguous member reference
```

**Example (invalid):**
```kestrel
class Bad {
    let x: Int
    let x: String      // ERROR: duplicate member 'x'
}

class AlsoBad {
    let process: Int
    func process() { }  // ERROR: duplicate member 'process'
}
```

### Rule 3: Function Overloading Allowed

Multiple functions with the same name are allowed if they have different signatures.

```kestrel
class Processor {
    func process(x: Int) { }
    func process(x: String) { }     // OK: different type
    func process(x: Int, y: Int) { } // OK: different arity
}
```

### Rule 4: Methods Must Have Bodies

All methods in a class must have implementations.

```
ERROR: FunctionBodyPass error
WHEN: Method declared without body
WHY: Classes require concrete implementations
```

**Example (invalid):**
```kestrel
class Service {
    func start()    // ERROR: function 'start' requires a body
}
```

### Rule 5: Static Members Allowed

Classes can have static fields and methods.

```kestrel
class Counter {
    static let MAX: Int
    static var count: Int

    static func increment() { }
    static func reset() { }
}
```

## Class as a Type

Classes create nominal types:

```kestrel
class MyClass { }

let instance: MyClass    // MyClass is a type
func accept(c: MyClass) { }
```

The class type is created during the build phase and attached via `TypedBehavior`.

## Class Scope

Classes create a scope containing:
- Fields (instance and static)
- Methods (instance and static)
- Nested types (classes, structs, protocols)

### Scope Hierarchy

```
Module scope
└── Class scope
    ├── Nested class scope
    ├── Nested struct scope
    └── Nested protocol scope
```

### Visibility Scope

For private members, the class is the visibility scope:

```kestrel
class Outer {
    private let secret: Int    // visibility_scope = Outer

    class Inner {
        // Can access Outer.secret because Inner is inside Outer
    }
}
```

## Member Access

Members are accessed via dot notation:

```kestrel
let obj: MyClass
obj.field           // Instance field
obj.method()        // Instance method

MyClass.staticField // Static field
MyClass.staticMethod() // Static method
```

## Formal Semantics

### Class Declaration

For `class C { members... }`:

```
Effect:
    - Creates ClassSymbol with name C
    - Creates type Ty::Class(Arc<ClassSymbol>)
    - Adds TypedBehavior with class type
    - Creates scope for C
    - Processes all members in C's scope

Scope:
    scope(C) = {
        declarations: {field names, method names, nested type names},
        parent: enclosing scope
    }
```

### Type Creation

```rust
let class_symbol = Arc::new(ClassSymbol::new(name, visibility));
let class_type = Ty::class(class_symbol.clone(), span);
class_symbol.add_behavior(TypedBehavior::new(class_type, span));
```

### Member Resolution

```
resolve_member(class, name):
    for member in class.members:
        if member.name == name:
            if is_visible(member, access_context):
                return member
    return NotFound
```

## Symbol Structure

```rust
ClassSymbol {
    name: String,
    visibility_behavior: VisibilityBehavior,
    typed_behavior: TypedBehavior,  // Contains Ty::Class(self)
    children: Vec<Symbol>,          // Fields, methods, nested types
}
```

## Source Location

- **Resolver:** `lib/kestrel-semantic-tree-builder/src/resolvers/class.rs`
- **Symbol:** `lib/kestrel-semantic-tree/src/symbol/class.rs`
- **Validation:** `lib/kestrel-semantic-tree-builder/src/validation/duplicate_symbol.rs`
