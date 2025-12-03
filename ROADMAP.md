# Kestrel Language Roadmap

## Phase 1: Type System Foundation

### Core Type System

- [x] Type Aliases - Define reusable type names (`type String = Array<Char>`)
  - [x] Parser support
  - [x] Semantic tree representation (TypeAliasSymbol)
  - [x] Type resolution (resolves aliased types)
  - [x] Circular alias detection
- [x] Import Resolution - Complete the ImportResolver
  - [x] Module path resolution
  - [x] Imported symbol lookup
  - [x] Specific imports `import A.(Foo, Bar)`
  - [x] Aliased imports `import A as B`, `import A.(Foo as F)`
  - [x] Whole-module imports `import A.B.C`
  - [x] Visibility checking (public/private/internal/fileprivate)
  - [x] Cross-file error reporting with precise spans
- [x] Type Resolution - Resolve type references across modules
  - [x] Path type resolution (`A.B.C` -> concrete type)
  - [x] Scope-aware name lookup
  - [x] Cross-module type references
- [x] Primitive Types - Int, Float, String, Bool (TyKind variants)

### Modules & Visibility (moved from Phase 5 - DONE)

- [x] Module declarations (`module A.B.C`)
- [x] Visibility modifiers (public, private, internal, fileprivate)
- [x] Visibility scope tracking
- [x] Cross-module visibility checking

### Classes (Temporary - will be replaced by Structs)

- [x] Class declarations with visibility
- [x] Nested classes
- [x] Class type representation

### Structured Types

- [x] Structs - Replace classes with lightweight data structures
  - [x] Parser support for struct declarations
  - [x] Semantic tree representation (StructSymbol)
  - [x] Struct type resolution (TyKind::Struct)
- [x] Struct Fields / Global Variables - `(visibility)? (static)? let/var name: Type`
  - [x] Parser support for field declarations
  - [x] Semantic tree representation (FieldSymbol)
  - [x] Static vs instance field tracking
  - [x] Mutability (let vs var)
  - [x] Works in struct bodies and at module level (globals)
- [x] Protocols - Define interfaces/contracts
  - [x] Parser support for protocol declarations
  - [x] Semantic tree representation (ProtocolSymbol)
  - [x] Protocol type resolution (TyKind::Protocol)
  - [x] Generic protocols with type parameters and where clauses
  - [x] Validation: protocol methods cannot have bodies
  - [x] Protocol inheritance (`protocol A: B { }`)
  - [x] Protocol conformance syntax (`struct Point: Drawable { }`)
  - [x] Conformance validation (check all methods implemented)

### Functions

- [x] Function Declarations - `(visibility)? (static)? fn name(params) (-> Type)? { }`
  - [x] Parser support for function declarations
  - [x] Function signatures with parameter types
  - [x] Return type declarations
  - [x] Labeled parameters (`fn greet(with name: String)`)
  - [x] Semantic tree representation (FunctionSymbol)
  - [x] CallableBehavior for callable semantics
- [x] Function Overloading
  - [x] Overloading by arity (different parameter counts)
  - [x] Overloading by parameter types
  - [x] Overloading by labels (labeled vs unlabeled)
  - [x] Duplicate signature detection with clear error messages
- [x] Function Types - First-class function types `(Int, Int) -> Int`
  - [x] Parser support for function type syntax
  - [x] TyKind::Function representation

### Type Expressions (Parser)

- [x] Unit type `()`
- [x] Never type `!`
- [x] Tuple types `(T1, T2, ...)`
- [x] Function types `(P1, P2) -> R`
- [x] Path types `A.B.C`

## Phase 2: Generics

- [x] Generic Type Parameters - `Struct[T]`, `Protocol[T]`
  - [x] Parser support for type parameter syntax
  - [x] TypeParameterSymbol representation
  - [x] Type parameter defaults `[T = Int]`
  - [x] Type argument application and arity checking
- [x] Generic Functions - `func identity[T](value: T) -> T`
  - [x] Parser support
  - [x] FunctionSymbol with type parameters
- [x] Generic Constraints - `where T: Protocol`
  - [x] Parser support for where clauses
  - [x] WhereClause representation with bounds
  - [x] Validation (bounds must be protocols, params must exist)
- [x] Type Substitutions - Replace type parameters with concrete types
  - [x] Substitutions system for generic instantiation
  - [x] Recursive substitution through complex types

## Phase 3: Values & Expressions

### Literals

- [x] Integer Literals - `42`, `0xFF`, `0b1010`
- [x] Float Literals - `3.14`, `1.0e10`
- [x] String Literals - `"hello"`, escape sequences
- [x] Bool Literals - `true`, `false`
- [x] Array Literals - `[1, 2, 3]`
- [x] Tuple Literals - `(1, 2, 3)`

### Paths

- [x] Paths - `a.b.c`
- [x] Resolving paths - resolve to a value
- [x] Symbols can have a value associated with them (ValueBehavior)

### Variables

- [x] Variable Declarations - `let x: Int = 42`
- [x] Mutable Variables - `var x: Int = 42`
- [x] Pattern-based bindings (Statement::Binding with Pattern)
- [ ] Type Inference - `let x = 42` (infer Int) [Deferred]
- [ ] Assignment Expressions - `x = 43`, `point.x = 10` (see Struct Operations)

### Function Operations

- [x] Function Calls - `add(1, 2)`, `module.function(arg)`
- [x] Calling overloaded functions (by arity + labels)
- [x] Method Calls - `obj.method(args)`
- [x] Primitive Method Calls - `5.toString()`, `"hello".length()`
- [x] Self Parameter Handling - Methods get `self` automatically
  - [x] ReceiverKind enum (Borrowing, Mutating, Consuming, Initializing)
  - [x] `mutating func` and `consuming func` syntax
  - [x] Auto-injection of `self` local in instance methods
  - [x] Self type resolution for member access
  - [x] Error for `self` in static methods and free functions
- [x] Call validation
  - [x] Error for undefined functions
  - [x] Error for wrong arity in calls
  - [x] Error for wrong labels in calls
  - [x] Error for calling instance method on type name
- [ ] First-class Function References - Functions as values (overloaded names error for now)

### Expressions

- [x] Member Access - `struct.field` (via MemberAccessBehavior)
- [x] Chained Member Access - `obj.method().field` (parser fix for postfix member access)
- [ ] Binary Operators - `+`, `-`, `*`, `/`, `%`
- [ ] Comparison Operators - `==`, `!=`, `<`, `>`, `<=`, `>=`
- [ ] Logical Operators - `and`, `or`, `!`
- [ ] Block Expressions - `{ stmt; stmt; expr }`

### Struct Operations

- [x] Struct Instantiation - `Point(x: 10, y: 20)`
  - [x] Implicit memberwise initializer (generated from fields)
  - [x] Labeled argument matching (field names in declaration order)
  - [x] TypeRef expression for struct names as callees
  - [x] Diagnostics for arity/label mismatches
- [x] Struct Initializers - `init() {}`
  - [x] Parser support for initializer declarations
  - [x] InitializerSymbol with CallableBehavior
  - [x] ReceiverKind::Initializing for self handling
  - [x] Explicit init suppresses implicit memberwise init
  - [ ] Initializer body resolution (requires assignment expressions)
- [x] Field Access - `point.x`, `point.y`
- [ ] Assignment Expressions - `x = 5`, `point.x = 10`

## Phase 4: Control Flow

- [ ] If Expressions - `if condition { ... } else { ... }`
- [ ] Match Expressions - Pattern matching
- [ ] While Loops - `while condition { ... }`
- [ ] For Loops - `for item in collection { ... }`

## Phase 5: Advanced Features

- [x] Methods - Functions on structs (basic support via function declarations in struct bodies)
- [ ] Protocol Implementations - `impl Protocol for Struct`
- [ ] Associated Types - Types within protocols
- [ ] Error Handling - Result types, error propagation
- [ ] Traits/Extensions - Add methods to existing types
- [ ] Default Parameters - `func foo(x: Int = 0)`
- [ ] Variadic Parameters - `func log(messages: String...)`
- [ ] Trailing Closures

## Phase 6: Polish & Optimization

- [ ] Type Inference Improvements
  - [ ] Generic Type Inference - Deduce type parameters from usage
  - [ ] Generic Constraint Enforcement - Verify bounds at call sites
- [x] Error Messages - Clear, actionable diagnostics
  - [x] Cross-file diagnostics
  - [x] Precise span reporting (name-level, not declaration-level)
  - [x] Contextual secondary labels
- [ ] IDE Support - LSP implementation
- [ ] Optimization Passes
- [ ] Standard Library

---

## Current Status

**Phase**: Phase 3 (Values & Expressions) - IN PROGRESS
**Progress**: Phase 1 & 2 complete. Phase 3 partially complete.

**Completed in Phase 3**:

- Literals (all types)
- Path resolution
- Variable declarations (let/var with Pattern-based bindings)
- Function calls with overload resolution
- Method calls on structs
- Primitive method calls
- Member/field access
- Self parameter handling (`self` injection, `mutating`/`consuming` modifiers)
- Call validation (undefined functions, arity, labels, instance vs static)
- Chained member access (`obj.method().field`)
- Struct instantiation with implicit memberwise init
- Struct initializer declarations (`init() {}`)
- Diagnostics for struct instantiation errors

**Next Tasks**:

1. Assignment expressions (`x = 5`, `point.x = 10`) - **Blocking**: init body tests
2. Binary operators (`+`, `-`, `*`, `/`, etc.)
3. Comparison operators (`==`, `!=`, `<`, `>`, etc.)
4. Logical operators (`and`, `or`, `!`)
5. If/else expressions
6. Block expressions

## Notes

- Structs replace classes for a simpler, more flexible type system
- Protocols provide interface abstraction without inheritance complexity
- Functions are first-class, enabling functional programming patterns
- Function overloading supported via arity, types, and labels
- Labeled parameters enable Swift-style named arguments
- Import resolution must work before advanced features (generics, protocols)
- Values/expressions come after types are solid
- Classes are implemented as a stepping stone; will be replaced by Structs
