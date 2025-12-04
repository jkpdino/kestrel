# Kestrel Test Suite Organization

This test suite is organized by semantic domain rather than syntax. Tests are grouped by what they validate, not by the surface-level language construct being tested.

## Directory Structure

```
tests/
├── mod.rs                              # Main test module
├── declarations/                       # Symbol Declarations (82 tests)
│   ├── structs.rs                      # Struct definitions, fields, nested structs (41 tests)
│   ├── functions.rs                    # Function declarations, overloading (15 tests)
│   ├── protocols.rs                    # Protocol definitions, inheritance (40 tests)
│   ├── type_aliases.rs                 # Type alias definitions (50 tests)
│   └── imports.rs                      # Import statements, visibility (30 tests)
│
├── types/                              # Type System (130 tests)
│   ├── generics.rs                     # Generic parameters, constraints, where clauses (80 tests)
│   └── literals.rs                     # Literal types and parsing (50 tests)
│
├── expressions/                        # Expression Resolution (310 tests)
│   ├── body_literals.rs                # Literal expressions in function bodies (12 tests)
│   ├── operators.rs                    # Binary/unary operators, precedence (60 tests)
│   ├── paths.rs                        # Path expressions, variable references (80 tests)
│   ├── field_access.rs                 # Struct field access (15 tests)
│   └── calls/                          # Call Expressions
│       ├── function_calls.rs           # Standalone function calls (30 tests)
│       └── method_calls.rs             # Instance/static method calls, self (113 tests)
│
├── statements/                         # Statement Resolution (15 tests)
│   ├── variables.rs                    # let/var declarations, shadowing (8 tests)
│   └── assignments.rs                  # Assignment statements (7 tests)
│
├── validation/                         # Semantic Validation (73 tests)
│   ├── mutability.rs                   # Mutable/immutable checks (20 tests)
│   ├── cycles.rs                       # Circular reference detection (20 tests)
│   └── misc.rs                         # Visibility, duplicates, conformance (33 tests)
│
├── instantiation/                      # Creating Instances (50 tests)
│   └── expressions.rs                  # Expression/statement data types
│
└── framework/                          # Test Framework Features (17 tests)
    └── test_suite_features.rs          # Path lookup, behaviors, expectations

Total: 627 tests (1 ignored)
```

## Organization Principles

### 1. Semantic Grouping
Tests are organized by **what** they validate, not **how** they're written:
- `declarations/structs.rs` - Testing struct *declarations*
- `instantiation/` - Testing struct *usage*
- `validation/` - Testing semantic *errors*

### 2. Progressive Nesting
- Top-level directories for major domains (declarations, types, expressions, etc.)
- Files within for specific features
- Subdirectories only for truly complex areas (e.g., `expressions/calls/`)

### 3. Clear Boundaries
Each directory has a focused responsibility:

| Directory | Purpose | What it validates |
|-----------|---------|-------------------|
| `declarations/` | Parsing & building symbols | Can we declare this construct? |
| `types/` | Type system features | Does the type system work correctly? |
| `expressions/` | Expression resolution | Do expressions resolve to the right values/types? |
| `statements/` | Statement resolution | Do statements execute correctly? |
| `validation/` | Semantic error checking | Are invalid programs rejected? |
| `instantiation/` | Using declared types | Can we create instances? |
| `framework/` | Test utilities | Does the test framework work? |

## Migration from Old Structure

The original `body_resolution.rs` (38KB, 110 tests) was split across multiple domains:

| Old module | New location | Rationale |
|-----------|--------------|-----------|
| `literal_expressions` | `expressions/body_literals.rs` | Expression domain |
| `composite_expressions` | `expressions/body_literals.rs` | Expression domain |
| `field_access` | `expressions/field_access.rs` | Expression domain |
| `function_calls` | `expressions/calls/function_calls.rs` | Call resolution |
| `method_calls` | `expressions/calls/method_calls.rs` | Call resolution |
| `self_parameter` | `expressions/calls/method_calls.rs` | Method-specific |
| `primitive_methods` | `expressions/calls/method_calls.rs` | Method-specific |
| `local_variables` | `statements/variables.rs` | Statement domain |
| `assignment_expressions` | `statements/assignments.rs` | Statement domain |

## Benefits of New Structure

1. **Easier to find tests** - Clear semantic organization
2. **Better scalability** - Add new features to appropriate domains
3. **Reduced cognitive load** - Each file focuses on one aspect
4. **Balanced file sizes** - No more 38KB monolithic files
5. **Clearer test intent** - Directory structure tells you what's being tested

## Running Tests

```bash
# Run all tests
cargo test -p kestrel-test-suite

# Run tests for a specific domain
cargo test -p kestrel-test-suite --test declarations
cargo test -p kestrel-test-suite --test expressions
cargo test -p kestrel-test-suite --test validation

# Run tests for a specific feature
cargo test -p kestrel-test-suite declarations::structs
cargo test -p kestrel-test-suite expressions::calls::method_calls
```
