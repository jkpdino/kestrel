# Errors

This document catalogs all semantic errors in Kestrel, organized by category.

## Error Format

Each error is documented as:

```
ERROR NAME
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
When:     Condition that triggers the error
Why:      Reason this is an error
Message:  The error message shown to users
Source:   File where the error is defined/emitted
```

---

## Module Errors

### NoModuleDeclarationError

```
When:     A source file contains zero module declarations
Why:      Every file must belong to a module for proper organization
Message:  "no module declaration found in file"
Source:   lib/kestrel-semantic-tree-builder/src/diagnostics.rs
```

**Example:**
```kestrel
// file.kes - missing module declaration
struct MyStruct { }    // ERROR
```

---

### ModuleNotFirstError

```
When:     Import or declaration appears before module declaration
Why:      Module context must be established first
Message:  "module declaration must be the first statement in the file"
Source:   lib/kestrel-semantic-tree-builder/src/diagnostics.rs
```

**Example:**
```kestrel
import Other.Module    // ERROR: before module

module MyApp
```

---

### MultipleModuleDeclarationsError

```
When:     File contains two or more module declarations
Why:      A file can only belong to one module
Message:  "multiple module declarations found ({count} total)"
Source:   lib/kestrel-semantic-tree-builder/src/diagnostics.rs
```

**Example:**
```kestrel
module First
module Second    // ERROR: multiple declarations
```

---

## Import Errors

### ModuleNotFoundError

```
When:     Import path segment cannot be resolved
Why:      Cannot import from non-existent module
Message:  "module '{partial_path}' not found"
Source:   lib/kestrel-semantic-tree/src/error.rs
```

**Fields:**
- `path`: Full import path
- `failed_segment_index`: Which segment failed (0-based)
- `path_span`: Span of entire path
- `failed_segment_span`: Span of failed segment

**Example:**
```kestrel
import NonExistent.Module    // ERROR: 'NonExistent' not found
import Real.Fake.Thing       // ERROR: 'Real.Fake' not found
```

---

### SymbolNotFoundInModuleError

```
When:     Specific import item doesn't exist in target module
Why:      Cannot import a symbol that doesn't exist
Message:  "symbol '{symbol}' not found in module '{module}'"
Source:   lib/kestrel-semantic-tree/src/error.rs
```

**Fields:**
- `symbol_name`: Name of missing symbol
- `module_path`: Path to the module
- `symbol_span`: Span of symbol name
- `module_span`: Span of module path

**Example:**
```kestrel
// Utils module has: Logger, Config
import Utils.(Logger, Missing)    // ERROR: 'Missing' not found
```

---

### CannotImportFromNonModuleError

```
When:     Import path resolves to non-module (struct, etc.)
Why:      Only modules can contain importable declarations
Message:  "cannot import from '{path}': not a module"
Source:   lib/kestrel-semantic-tree/src/error.rs
```

**Fields:**
- `symbol_kind`: Kind of symbol found (e.g., "struct")
- `path`: The import path
- `path_span`: Span of the path

**Example:**
```kestrel
// Other.MyClass is a struct, not a module
import Other.MyClass    // ERROR: cannot import from struct
```

---

### SymbolNotVisibleError

```
When:     Imported symbol has insufficient visibility
Why:      Cannot access private/internal symbols from outside
Message:  "'{symbol}' is not accessible"
Note:     Shows actual visibility level
Source:   lib/kestrel-semantic-tree/src/error.rs
```

**Fields:**
- `symbol_name`: Name of inaccessible symbol
- `visibility`: The symbol's visibility level
- `import_span`: Span of import statement
- `declaration_span`: Span of symbol declaration
- `declaration_file_id`: File containing declaration

**Example:**
```kestrel
// In module A:
private struct Secret { }

// In module B:
import A.(Secret)    // ERROR: 'Secret' is not accessible (private)
```

---

### ImportConflictError

```
When:     Whole-module import introduces conflicting name
Why:      Ambiguous names cause confusion
Message:  "'{name}' is already {declared|imported}"
Source:   lib/kestrel-semantic-tree/src/error.rs
```

**Fields:**
- `name`: Conflicting name
- `import_span`: Span of import statement
- `existing_span`: Span of existing declaration/import
- `existing_is_import`: Whether conflict is with another import

**Example:**
```kestrel
struct Logger { }
import Utils    // Utils has Logger -> ERROR: 'Logger' already declared
```

---

## Type Alias Errors

### CircularTypeAliasError

```
When:     Type alias forms a circular reference chain
Why:      Would create infinite type with no base
Message:  "circular type alias: {origin} -> {chain} -> {origin}"
Source:   lib/kestrel-semantic-tree/src/error.rs
```

**Fields:**
- `origin`: The type alias where cycle was detected
- `cycle`: List of type aliases in the cycle chain

**Example:**
```kestrel
type A = B;
type B = C;
type C = A;    // ERROR: circular type alias: A -> B -> C -> A
```

---

## Function Errors

### Function Requires Body

```
When:     Function outside protocol has no body
Why:      Non-protocol functions need implementations
Message:  "function '{name}' requires a body"
Source:   lib/kestrel-semantic-tree-builder/src/validation/function_body.rs
```

**Example:**
```kestrel
func compute() -> Int    // ERROR: requires body

struct Service {
    func process()       // ERROR: requires body
}
```

---

### Protocol Method Cannot Have Body

```
When:     Method inside protocol has a body
Why:      Protocols define interfaces, not implementations
Message:  "protocol method '{name}' cannot have a body"
Source:   lib/kestrel-semantic-tree-builder/src/validation/protocol_method.rs
```

**Example:**
```kestrel
protocol Runnable {
    func run() { }    // ERROR: cannot have body
}
```

---

### Duplicate Function Signature

```
When:     Two functions have identical signatures in same scope
Why:      Would create ambiguity during overload resolution
Message:  "duplicate function signature: {signature}"
Source:   lib/kestrel-semantic-tree-builder/src/lib.rs
```

**Example:**
```kestrel
func process(x: Int) { }
func process(x: Int) { }    // ERROR: duplicate signature
```

---

### Static Modifier Invalid Context

```
When:     static used outside struct/protocol
Why:      static only meaningful inside types
Message:  "static modifier is only allowed inside struct or protocol"
Source:   lib/kestrel-semantic-tree-builder/src/validation/static_context.rs
```

**Example:**
```kestrel
module App

static func utility() { }    // ERROR: invalid context
```

---

## Duplicate Symbol Errors

### Duplicate Type Name

```
When:     Two types have same name in same scope
Why:      Ambiguous type reference
Message:  "duplicate type '{name}': already defined as {kind}"
Source:   lib/kestrel-semantic-tree-builder/src/validation/duplicate_symbol.rs
```

**Example:**
```kestrel
struct Item { }
struct Item { }    // ERROR: duplicate type 'Item'
```

---

### Duplicate Member Name

```
When:     Two members have same name in a type
Why:      Ambiguous member reference
Message:  "duplicate member '{name}' in {kind} '{type}': already defined as {member_kind}"
Source:   lib/kestrel-semantic-tree-builder/src/validation/duplicate_symbol.rs
```

**Example:**
```kestrel
struct Bad {
    let x: Int
    let x: String    // ERROR: duplicate member 'x'
}

struct AlsoBad {
    let value: Int
    func value() { }  // ERROR: duplicate member 'value'
}
```

---

## Visibility Errors

### Public Exposes Private Return Type

```
When:     Public function returns less-visible type
Why:      Callers couldn't use the return type
Message:  "public function '{name}' exposes {visibility} type '{type}'"
Source:   lib/kestrel-semantic-tree-builder/src/validation/visibility_consistency.rs
```

**Example:**
```kestrel
private struct Secret { }
public func getSecret() -> Secret { }    // ERROR
```

---

### Public Exposes Private Parameter Type

```
When:     Public function has less-visible parameter type
Why:      Callers couldn't provide required arguments
Message:  "public function '{name}' exposes {visibility} type '{type}' in parameter"
Source:   lib/kestrel-semantic-tree-builder/src/validation/visibility_consistency.rs
```

**Example:**
```kestrel
private struct Config { }
public func configure(c: Config) { }    // ERROR
```

---

### Public Type Alias Exposes Private Type

```
When:     Public type alias targets less-visible type
Why:      Users couldn't access the underlying type
Message:  "public type alias '{name}' exposes {visibility} type '{type}'"
Source:   lib/kestrel-semantic-tree-builder/src/validation/visibility_consistency.rs
```

**Example:**
```kestrel
private struct Impl { }
public type API = Impl;    // ERROR
```

---

### Public Field Exposes Private Type

```
When:     Public field has less-visible type
Why:      Users couldn't work with the field value
Message:  "public field '{name}' exposes {visibility} type '{type}'"
Source:   lib/kestrel-semantic-tree-builder/src/validation/visibility_consistency.rs
```

**Example:**
```kestrel
private struct Data { }
public struct Container {
    public let data: Data    // ERROR
}
```

---

## Type Resolution Errors

### Type Not Found

```
When:     Type path segment doesn't exist
Why:      Cannot use undefined type
Message:  (context-dependent, from type resolution)
Source:   lib/kestrel-semantic-tree-builder/src/db.rs
```

**Result:** `TypePathResolution::NotFound { segment, index }`

**Example:**
```kestrel
let x: NonExistent    // ERROR: type not found
let y: A.B.Missing    // ERROR: 'Missing' not found at index 2
```

---

### Ambiguous Type

```
When:     Multiple symbols match type path segment
Why:      Cannot determine which type is intended
Message:  (context-dependent, from type resolution)
Source:   lib/kestrel-semantic-tree-builder/src/db.rs
```

**Result:** `TypePathResolution::Ambiguous { segment, index, candidates }`

---

### Not a Type

```
When:     Path resolves to non-type symbol
Why:      Expected type, got function/import/etc.
Message:  (context-dependent, from type resolution)
Source:   lib/kestrel-semantic-tree-builder/src/db.rs
```

**Result:** `TypePathResolution::NotAType { symbol_id }`

**Example:**
```kestrel
func helper() { }
let x: helper    // ERROR: 'helper' is not a type
```

---

## Error Summary Table

| Category | Error | Severity |
|----------|-------|----------|
| **Module** | NoModuleDeclarationError | Fatal |
| **Module** | ModuleNotFirstError | Fatal |
| **Module** | MultipleModuleDeclarationsError | Fatal |
| **Import** | ModuleNotFoundError | Error |
| **Import** | SymbolNotFoundInModuleError | Error |
| **Import** | CannotImportFromNonModuleError | Error |
| **Import** | SymbolNotVisibleError | Error |
| **Import** | ImportConflictError | Error |
| **Type Alias** | CircularTypeAliasError | Error |
| **Function** | Function requires body | Error |
| **Function** | Protocol method has body | Error |
| **Function** | Duplicate signature | Error |
| **Function** | Static invalid context | Error |
| **Duplicate** | Duplicate type name | Error |
| **Duplicate** | Duplicate member name | Error |
| **Visibility** | Public exposes private (return) | Error |
| **Visibility** | Public exposes private (param) | Error |
| **Visibility** | Public exposes private (alias) | Error |
| **Visibility** | Public exposes private (field) | Error |
| **Type** | Type not found | Error |
| **Type** | Ambiguous type | Error |
| **Type** | Not a type | Error |

---

## Diagnostic Structure

All errors are converted to diagnostics with:

```rust
struct Diagnostic {
    severity: Severity,        // Error, Warning, Info
    message: String,           // Primary message
    labels: Vec<Label>,        // Source locations
    notes: Vec<String>,        // Additional information
}

struct Label {
    span: Span,
    message: Option<String>,
    file_id: FileId,           // Supports cross-file diagnostics
    style: LabelStyle,         // Primary or Secondary
}
```

### Cross-File Diagnostics

Some errors reference multiple files:

```kestrel
// file_a.kes
module A
private struct Secret { }

// file_b.kes
module B
import A.(Secret)    // ERROR with labels in both files
```

The error shows:
- Primary label: Import location in file_b.kes
- Secondary label: Declaration location in file_a.kes

---

## Source Files

| Category | Source File |
|----------|-------------|
| Module errors | `lib/kestrel-semantic-tree-builder/src/diagnostics.rs` |
| Import errors | `lib/kestrel-semantic-tree/src/error.rs` |
| Type alias errors | `lib/kestrel-semantic-tree/src/error.rs` |
| Function body | `lib/kestrel-semantic-tree-builder/src/validation/function_body.rs` |
| Protocol method | `lib/kestrel-semantic-tree-builder/src/validation/protocol_method.rs` |
| Static context | `lib/kestrel-semantic-tree-builder/src/validation/static_context.rs` |
| Duplicate symbols | `lib/kestrel-semantic-tree-builder/src/validation/duplicate_symbol.rs` |
| Visibility | `lib/kestrel-semantic-tree-builder/src/validation/visibility_consistency.rs` |
| Type resolution | `lib/kestrel-semantic-tree-builder/src/db.rs` |
| Diagnostics base | `lib/kestrel-reporting/src/` |
