# Kestrel Parser Organization Principles

This document outlines the organizational principles for the kestrel-parser crate. Follow these principles when adding new declaration types or modifying existing parsers.

## Principle 1: Single Source of Truth for Each Declaration Type

Each declaration type should have its parser and emitter defined in **one place** - its own module.

| Declaration Type | Module | Owns Parser | Owns Emitter |
|------------------|--------|-------------|--------------|
| Struct | `struct/mod.rs` | ✓ | ✓ |
| Protocol | `protocol/mod.rs` | ✓ | ✓ |
| Function | `function/mod.rs` | ✓ | ✓ |
| Field | `field/mod.rs` | ✓ | ✓ |
| TypeAlias | `type_alias/mod.rs` | ✓ | ✓ |
| Module | `module/mod.rs` | ✓ | ✓ |
| Import | `import/mod.rs` | ✓ | ✓ |

**Do NOT** duplicate parsing or emitting logic across modules.

## Principle 2: `common/` Contains Shared Building Blocks

The `common/` module provides reusable components:

- **`common/parsers.rs`**: Low-level Chumsky parser combinators
  - `visibility_parser_internal()` - parses visibility modifiers
  - `static_parser()` - parses optional static keyword
  - `parameter_list_parser()` - parses function parameters
  - `identifier()`, `token()`, `skip_trivia()` - basic utilities

- **`common/emitters.rs`**: Shared event emission functions
  - `emit_visibility()` - emits Visibility node
  - `emit_name()` - emits Name node wrapping identifier
  - `emit_static_modifier()` - emits StaticModifier node
  - `emit_parameter_list()` - emits ParameterList with Parameters

- **`common/data.rs`**: Shared data structures
  - `ParameterData` - parsed parameter info
  - Other shared intermediate representations

**Rule**: If a parser or emitter is used by 2+ modules, it belongs in `common/`.

## Principle 3: `declaration_item/` is a Router, Not an Implementer

The `declaration_item/` module:

1. **Routes** to the appropriate module's parser based on the leading token(s)
2. **Aggregates** all declaration types into `DeclarationItem` enum
3. **Does NOT** contain its own parsing or emitting implementations

```rust
// GOOD: declaration_item routes to module-specific parsers
let struct_parser = struct::struct_declaration_parser_internal()
    .map(DeclarationItemData::Struct);

// BAD: declaration_item implements struct parsing itself
let struct_parser = visibility_parser_internal()
    .then(token(Token::Struct))
    .then(identifier())
    // ... duplicating struct/mod.rs logic
```

## Principle 4: Each Module Exports Its Parser and Emitter

Each declaration module should export:

1. **Parser**: `{name}_declaration_parser_internal()` - returns the parsed data
2. **Emitter**: `emit_{name}_declaration()` - emits events to EventSink
3. **Data type**: `{Name}DeclarationData` - intermediate representation (if needed externally)
4. **Public API**: `parse_{name}_declaration()` - high-level parse function

Example for struct:
```rust
// struct/mod.rs exports:
pub(crate) fn struct_declaration_parser_internal() -> impl Parser<...>
pub(crate) fn emit_struct_declaration(sink: &mut EventSink, data: StructDeclarationData)
pub fn parse_struct_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
```

## Principle 5: Body Content is Module-Specific

Each declaration type defines what can appear in its body:

| Declaration | Body Contains |
|-------------|---------------|
| Struct | Fields, Functions, nested Structs, Modules, Imports |
| Protocol | Functions (methods only, typically without bodies) |
| Function | Statements/expressions (future) |
| Module | N/A (no body) |
| Import | N/A (no body) |
| TypeAlias | N/A (no body) |
| Field | N/A (no body) |

Each module with a body uses `declaration_item/` to parse nested items, creating a dependency:
- `struct/` depends on `declaration_item/` for body parsing
- `protocol/` depends on `function/` for method parsing
- `declaration_item/` depends on all specific modules

## Principle 6: Consistent Generics Support

All declarations that support generics should parse them consistently:

```rust
// Type parameters: [T, U, V]
type_parameter_list_parser()

// Where clause: where T: Protocol, U: OtherProtocol
where_clause_parser()
```

Declarations with generics support:
- Struct ✓
- Protocol ✓
- Function ✓
- TypeAlias ✓

## Module Dependency Graph

```
declaration_item
    ├── struct (uses declaration_item for body)
    ├── protocol (uses function for methods)
    ├── function
    ├── field
    ├── type_alias
    ├── module
    └── import

common (used by all)
    ├── parsers
    ├── emitters
    └── data

type_param (used by struct, protocol, function, type_alias)
ty (used by field, function, type_alias, type_param)
```

## Adding a New Declaration Type

1. Create a new module: `new_type/mod.rs`
2. Define `NewTypeDeclaration` struct with syntax tree
3. Define `NewTypeDeclarationData` for intermediate representation
4. Implement `new_type_declaration_parser_internal()`
5. Implement `emit_new_type_declaration()`
6. Implement `parse_new_type_declaration()`
7. Add variant to `DeclarationItem` in `declaration_item/mod.rs`
8. Add routing in `declaration_item_parser_internal()`
9. Add tests

## File Structure

```
kestrel-parser/src/
├── lib.rs                 # Crate root, re-exports
├── parser.rs              # High-level Parser struct
├── event.rs               # Event-based parsing infrastructure
├── common/
│   ├── mod.rs             # Re-exports
│   ├── data.rs            # Shared data structures
│   ├── parsers.rs         # Shared parser combinators
│   └── emitters.rs        # Shared event emitters
├── declaration_item/
│   └── mod.rs             # Router + DeclarationItem enum
├── struct/
│   └── mod.rs             # Struct parsing (owns all struct logic)
├── protocol/
│   └── mod.rs             # Protocol parsing (owns all protocol logic)
├── function/
│   └── mod.rs             # Function parsing (owns all function logic)
├── field/
│   └── mod.rs             # Field parsing (owns all field logic)
├── type_alias/
│   └── mod.rs             # TypeAlias parsing
├── module/
│   ├── mod.rs             # Module declaration parsing
│   └── path.rs            # Module path parsing
├── import/
│   └── mod.rs             # Import declaration parsing
├── type_param/
│   └── mod.rs             # Type parameters and where clauses
└── ty/
    └── mod.rs             # Type expressions
```
