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
- [ ] Type Checking Infrastructure - Core type validation logic

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
- [ ] Protocols - Define interfaces/contracts
  - [ ] Parser support for protocol declarations
  - [ ] Protocol type checking
  - [ ] Protocol conformance validation

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

- [ ] Generic Type Parameters - `Struct[T]`, `Protocol<T>`
- [ ] Generic Functions - `fn identity[T](value: T) -> T`
- [ ] Generic Constraints - `where T: Protocol`
- [ ] Generic Type Inference - Deduce type parameters from usage

## Phase 3: Values & Expressions

### Literals

- [ ] Integer Literals - `42`, `0xFF`, `0b1010`
- [ ] Float Literals - `3.14`, `1.0e10`
- [ ] String Literals - `"hello"`, escape sequences
- [ ] Bool Literals - `true`, `false`
- [ ] Array Literals - `[1, 2, 3]`

### Struct Operations

- [ ] Struct Initialization - `Point { x: 10, y: 20 }`
- [ ] Field Access - `point.x`, `point.y`
- [ ] Struct Field Assignment - `point.x = 5`

### Function Operations

- [ ] Function Calls - `add(1, 2)`, `module.function(arg)`
- [ ] Function References - Functions as values
- [ ] Closures - Anonymous functions (stretch goal)

### Variables

- [ ] Variable Declarations - `let x: Int = 42`
- [ ] Mutable Variables - `var x: Int = 42`
- [ ] Type Inference - `let x = 42` (infer Int)
- [ ] Variable Reassignment - `x = 43`

### Expressions

- [ ] Arithmetic Operations - `+`, `-`, `*`, `/`, `%`
- [ ] Comparison Operations - `==`, `!=`, `<`, `>`, `<=`, `>=`
- [ ] Logical Operations - `&&`, `||`, `!`
- [ ] Member Access - `struct.field`, `module.function`

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

## Phase 6: Polish & Optimization

- [ ] Type Inference Improvements
- [x] Error Messages - Clear, actionable diagnostics
  - [x] Cross-file diagnostics
  - [x] Precise span reporting (name-level, not declaration-level)
  - [x] Contextual secondary labels
- [ ] IDE Support - LSP implementation
- [ ] Optimization Passes
- [ ] Standard Library

---

## Current Status

**Phase**: Phase 1 (Type System Foundation)
**Progress**: ~90% of Phase 1 complete
**Next Tasks**:

1. Protocols (medium)
2. Type Checking Infrastructure

## Notes

- Structs replace classes for a simpler, more flexible type system
- Protocols provide interface abstraction without inheritance complexity
- Functions are first-class, enabling functional programming patterns
- Function overloading supported via arity, types, and labels
- Labeled parameters enable Swift-style named arguments
- Import resolution must work before advanced features (generics, protocols)
- Values/expressions come after types are solid
- Classes are implemented as a stepping stone; will be replaced by Structs
