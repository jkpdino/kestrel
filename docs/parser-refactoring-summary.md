# Parser Refactoring Summary

## Overview

Refactored the Kestrel parser to better integrate Chumsky and Rowan, creating a cleaner, more efficient architecture where syntax trees are the single source of truth.

## Key Improvements

### 1. **Single Source of Truth: The Syntax Tree**

**Before:**
```rust
pub struct ModulePath {
    pub segments: Vec<Span>,  // Data duplicated
    pub syntax: SyntaxNode,   // Tree duplicated
}
```

**After:**
```rust
pub struct ModulePath {
    pub syntax: SyntaxNode,  // Single source of truth
}

impl ModulePath {
    pub fn segment_names(&self) -> Vec<String> {
        // Derive from syntax tree
        self.identifier_tokens()
            .map(|tok| tok.text().to_string())
            .collect()
    }
}
```

**Benefits:**
- ✅ No data duplication
- ✅ Cannot get out of sync
- ✅ Lossless representation
- ✅ Easy to add new accessors

### 2. **Eager Tree Construction**

**Before** (Two-pass):
```rust
// Step 1: Parse to spans
let (module_span, path_segments) = module_declaration_parser().parse(tokens)?;

// Step 2: Build tree from spans
let decl = parse_module_declaration(source, module_span, path_segments, full_span);
```

**After** (Single-pass):
```rust
// Build tree during parsing
let decl = module_declaration_parser(source).parse(tokens)?;
```

**Benefits:**
- ✅ Simpler API
- ✅ More efficient (one pass)
- ✅ Tree built immediately
- ✅ Less error-prone

### 3. **Better Encapsulation**

**Before:**
```rust
// Exposed implementation details
pub fn parse_module_path(source: &str, segments: Vec<Span>) -> ModulePath

// Users had to call two functions
let spans = module_path_parser().parse(tokens)?;
let path = parse_module_path(source, spans);
```

**After:**
```rust
// Constructor is private, parser returns complete structure
pub fn module_path_parser(source: &'static str) -> impl Parser<Token, ModulePath>

// Users call one function
let path = module_path_parser(source).parse(tokens)?;
```

**Benefits:**
- ✅ Implementation details hidden
- ✅ Cannot misuse API
- ✅ Simpler for users

### 4. **Derived Accessors**

All data is derived from the syntax tree rather than stored:

```rust
impl ModulePath {
    /// Get identifier tokens
    pub fn identifier_tokens(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .filter(|tok| tok.kind() == SyntaxKind::Identifier)
    }

    /// Get segment names
    pub fn segment_names(&self) -> Vec<String> {
        self.identifier_tokens()
            .map(|tok| tok.text().to_string())
            .collect()
    }

    /// Get segment count
    pub fn segment_count(&self) -> usize {
        self.identifier_tokens().count()
    }
}
```

This pattern makes it trivial to add new accessors without changing the data structure.

### 5. **Composition Through Syntax Tree**

```rust
impl ModuleDeclaration {
    /// Get the module path from this declaration
    pub fn path(&self) -> ModulePath {
        self.syntax
            .children()
            .find(|node| node.kind() == SyntaxKind::ModulePath)
            .map(|node| ModulePath { syntax: node })
            .expect("ModuleDeclaration must have a ModulePath child")
    }
}
```

Parent structures can create child structures by extracting syntax nodes.

## Migration Guide

### Old API

```rust
use kestrel_parser::{module_path_parser, parse_module_path};

let segments = module_path_parser().parse(tokens)?;
let path = parse_module_path(source, segments);

// Access data
let names = path.segment_names(source);
```

### New API

```rust
use kestrel_parser::module_path_parser;

let path = module_path_parser(source).parse(tokens)?;

// Access data (no source needed)
let names = path.segment_names();
```

## Architecture Principles

1. **Syntax Tree is the Source of Truth**: Store the lossless syntax tree, derive everything else
2. **Eager Construction**: Build trees during parsing, not after
3. **Private Constructors**: Users should only create structures through parsers
4. **Derived Accessors**: Add methods that query the syntax tree rather than storing data
5. **Composition via Nodes**: Extract child nodes to create child structures

## Performance Characteristics

- **Space**: Slightly better (no duplicate data)
- **Time**: Similar or better (one pass instead of two)
- **Ergonomics**: Much better (simpler API, fewer function calls)
- **Correctness**: Better (impossible to have inconsistent data)

## Future Extensions

This architecture makes it easy to:
- Add new accessor methods without changing structures
- Support incremental reparsing (Rowan feature)
- Build IDE features (code completion, go-to-definition) on top of the syntax tree
- Add span queries and source location tracking through the tree
