# Kestrel Language Roadmap

## Phase 1: Type System Foundation

### Core Type System

- [ ] Type Aliases - Define reusable type names (`type String = Array<Char>`)
- [ ] Import Resolution - Complete the ImportResolver
  - [ ] Module path resolution
  - [ ] Imported symbol lookup
  - [ ] Import cycle detection
  - [ ] Cross-module type references
- [ ] Type Resolution - Resolve type references across modules
- [ ] Primitive Types - Int, Float, String, Bool, etc.
- [ ] Type Checking Infrastructure - Core type validation logic

### Structured Types

- [ ] Structs - Replace classes with lightweight data structures
  - [ ] Parser support for struct declarations
  - [ ] Semantic tree representation
  - [ ] Struct type resolution
- [ ] Protocols - Define interfaces/contracts
  - [ ] Parser support for protocol declarations
  - [ ] Protocol type checking
  - [ ] Protocol conformance validation

### Functions

- [ ] Function Declarations - Standalone functions (not methods yet)
  - [ ] Parser support (reimplement properly)
  - [ ] Function signatures with parameter types
  - [ ] Return type declarations
  - [ ] Function type resolution
- [ ] Function Types - First-class function types `(Int, Int) -> Int`

## Phase 2: Generics

- [ ] Generic Type Parameters - `Struct<T>`, `Protocol<T>`
- [ ] Generic Functions - `fn identity<T>(value: T) -> T`
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

- [ ] Methods - Functions on structs
- [ ] Protocol Implementations - `impl Protocol for Struct`
- [ ] Associated Types - Types within protocols
- [ ] Modules & Visibility - Public/private declarations
- [ ] Error Handling - Result types, error propagation
- [ ] Traits/Extensions - Add methods to existing types

## Phase 6: Polish & Optimization

- [ ] Type Inference Improvements
- [ ] Error Messages - Clear, actionable diagnostics
- [ ] IDE Support - LSP implementation
- [ ] Optimization Passes
- [ ] Standard Library

---

## Current Status

**Phase**: Phase 1 (Type System Foundation)
**Next Task**: Type Aliases

## Notes

- Structs replace classes for a simpler, more flexible type system
- Protocols provide interface abstraction without inheritance complexity
- Functions are first-class, enabling functional programming patterns
- Import resolution must work before advanced features (generics, protocols)
- Values/expressions come after types are solid
