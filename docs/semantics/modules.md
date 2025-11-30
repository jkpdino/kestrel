# Modules

Modules are the primary organizational unit in Kestrel. Every source file must declare which module it belongs to.

## Syntax

```
ModuleDeclaration → MODULE ModulePath

ModulePath → Identifier (DOT Identifier)*
```

### Tokens
- `MODULE` - The `module` keyword
- `DOT` - The `.` character
- `Identifier` - A valid identifier (Unicode XID_Start followed by XID_Continue*)

## Examples

```kestrel
// Simple module
module MyApp

// Nested module path
module MyApp.Core.Utils

// Unicode identifiers allowed
module café.αβγ
```

## Semantic Rules

### Rule 1: Module Declaration Required

Every Kestrel source file MUST contain exactly one module declaration.

```
ERROR: NoModuleDeclarationError
WHEN: A source file contains zero module declarations
WHY: Files must be organized into modules for proper scoping and imports
```

**Example (invalid):**
```kestrel
// ERROR: no module declaration found in file
class MyClass { }
```

### Rule 2: Module Declaration Must Be First

The module declaration MUST appear before any imports or other declarations.

```
ERROR: ModuleNotFirstError
WHEN: Any import or declaration appears before the module declaration
WHY: The module context must be established before processing other declarations
```

**Example (invalid):**
```kestrel
import Other.Module    // ERROR: module declaration must be first

module MyApp
```

**Example (valid):**
```kestrel
module MyApp

import Other.Module    // OK: after module declaration
```

### Rule 3: Single Module Declaration

Only ONE module declaration is allowed per source file.

```
ERROR: MultipleModuleDeclarationsError
WHEN: A source file contains two or more module declarations
WHY: A file can only belong to one module
```

**Example (invalid):**
```kestrel
module MyApp
module OtherApp    // ERROR: multiple module declarations found (2 total)
```

## Module Hierarchy

Module paths establish a hierarchy:

```kestrel
module A           // Root module A
module A.B         // Submodule B of A
module A.B.C       // Submodule C of A.B
```

Each segment creates a nested scope. The path `A.B.C` means:
- Module `C` is a child of module `B`
- Module `B` is a child of module `A`

## Module Scopes

Modules create scopes that contain:
- Imports (names from other modules)
- Declarations (classes, structs, protocols, functions, type aliases, nested modules)

Declarations within a module are visible to:
- Other declarations in the same module (subject to visibility)
- Declarations in other modules (if visibility allows)

## Module Identity

Two modules are the same if and only if their full paths are identical:

```kestrel
// File 1
module A.B.C

// File 2
module A.B.C    // Same module as File 1

// File 3
module A.B      // Different module (parent of A.B.C)
```

Multiple files can declare the same module path, contributing declarations to the same logical module.

## Formal Semantics

Let `M` be a module declaration with path `p₁.p₂...pₙ`.

**ModuleSymbol Creation:**
1. For each segment `pᵢ`, ensure a ModuleSymbol exists
2. Establish parent-child relationships: `parent(pᵢ) = pᵢ₋₁`
3. The final segment `pₙ` is the declared module

**Scope Creation:**
```
scope(M) = {
    imports: ∅,           // Initially empty, populated by imports
    declarations: {...},   // Populated by declarations in file
    parent: scope(pₙ₋₁)   // Parent module's scope, or root if n=1
}
```

## Source Location

- **Defined in:** `lib/kestrel-semantic-tree-builder/src/resolvers/module.rs`
- **Errors defined in:** `lib/kestrel-semantic-tree-builder/src/diagnostics.rs`
- **Symbol type:** `ModuleSymbol` in `lib/kestrel-semantic-tree/src/symbol/module.rs`
