# Kestrel Language Semantics

This documentation describes the formal semantics of the Kestrel programming language, including syntactic rules, semantic constraints, type resolution, and error conditions.

## Overview

Kestrel is a statically-typed language with:
- Module-based organization
- Four visibility levels (public, internal, fileprivate, private)
- Swift-style labeled function parameters
- Function overloading
- Protocols for interface definitions
- Classes and structs for data types
- Type aliases

## Compilation Phases

### Phase 1: Lexing
Converts source text into tokens. See individual construct documentation for token definitions.

### Phase 2: Parsing
Converts tokens into a syntax tree. The parser uses error recovery to continue after syntax errors.

### Phase 3: Semantic Analysis (Build)
Creates semantic symbols from syntax nodes:
- Extracts declarations (classes, structs, functions, etc.)
- Creates symbol table entries
- Stores unresolved type paths

### Phase 4: Semantic Analysis (Bind)
Resolves references and validates constraints:
- Resolves type paths to concrete types
- Validates imports
- Detects circular type aliases
- Checks duplicate signatures

### Phase 5: Validation
Runs validation passes:
1. `FunctionBodyPass` - Functions outside protocols need bodies
2. `ProtocolMethodPass` - Protocol methods cannot have bodies
3. `StaticContextPass` - Static modifier context validation
4. `DuplicateSymbolPass` - Duplicate type/member detection
5. `VisibilityConsistencyPass` - Public API consistency

## Documentation Index

### Core Constructs
- [Modules](modules.md) - Module declarations and organization
- [Imports](imports.md) - Import statements and module access
- [Types](types.md) - Type system overview
- [Type Aliases](type-aliases.md) - Type alias declarations

### Declarations
- [Functions](functions.md) - Function declarations and overloading
- [Classes](classes.md) - Class declarations
- [Structs](structs.md) - Struct declarations
- [Protocols](protocols.md) - Protocol declarations
- [Fields](fields.md) - Field declarations

### Resolution & Visibility
- [Visibility](visibility.md) - Access control system
- [Name Resolution](name-resolution.md) - How names are resolved
- [Type Resolution](type-resolution.md) - How types are resolved

### Reference
- [Errors](errors.md) - Complete error catalog

## Notation

Throughout this documentation:

- `→` denotes grammar production rules
- `|` denotes alternatives
- `?` denotes optional elements
- `*` denotes zero or more repetitions
- `+` denotes one or more repetitions
- `CAPS` denotes terminal tokens/keywords
- `lowercase` denotes non-terminal productions
- `"literal"` denotes literal syntax

Error conditions are shown as:

```
ERROR: ErrorName
WHEN: Condition that triggers the error
WHY: Explanation of why this is an error
```
