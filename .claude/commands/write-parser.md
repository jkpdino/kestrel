---
description: Implement a language feature by adding tokens to the lexer and creating parsers. Takes a feature document path (e.g., docs/module.md) and implements it following Kestrel's conventions.
---

You are implementing a new language feature for the Kestrel compiler. You will read a feature specification document and implement both the lexer tokens and parser for that feature.

# Architecture Overview

Kestrel uses a modern **event-driven parser architecture**:

- **Lexer** (`kestrel-lexer`): Tokenizes source code using Logos
- **Parser** (`kestrel-parser`): Event-driven parsers using Chumsky + event emission
- **Syntax Tree** (`kestrel-syntax-tree`): Lossless CST using Rowan library
- **Parser API**: High-level `Parser::parse()` returns `ParseResult` with tree + errors

Key concepts:
- Parsers **emit events** (StartNode, AddToken, FinishNode, Error) instead of returning AST
- Events are converted to **lossless syntax trees** that preserve all source text
- The `Parser` struct provides a convenient high-level API
- Multiple declarations can be parsed into a single `SourceFile` tree

# Step 1: Understand the Feature

Read the provided feature document carefully. Identify:
- **Keywords**: New reserved words (e.g., `module`, `fn`, `if`)
- **Literals**: New literal types (strings, numbers, etc.)
- **Operators**: New operators or punctuation (e.g., `->`, `::`, `..`)
- **Syntax structures**: How these tokens combine (e.g., `module A.B.C`)

# Step 2: Add Tokens to the Lexer

File: `lib/kestrel-lexer/src/lib.rs`

## Token Categories

The lexer organizes tokens into **strict categories** in this order:

1. **Comments** (skip patterns at top)
   ```rust
   #[logos(skip r"[ \t\n\f]+")]  // Whitespace
   #[logos(skip r"//[^\n]*")]     // Line comments
   ```

2. **Literals** (always first in the enum)
   - `Identifier` - Unicode identifiers with XID validation
   - `String` - String literals
   - `Integer` - Integer literals
   - `Float` - Float literals
   - `Boolean` - true/false
   - `Null` - null keyword

3. **Declaration Keywords**
   - Keywords that declare things: `module`, `fn`, `let`, `type`, `struct`, etc.
   - Order alphabetically within this section

4. **Statement Keywords**
   - Keywords for control flow: `if`, `else`, `while`, `for`, `match`, `return`, etc.
   - Order alphabetically within this section

5. **Braces**
   - `LParen` / `RParen` - `(` and `)`
   - `LBrace` / `RBrace` - `{` and `}`
   - `LBracket` / `RBracket` - `[` and `]`

6. **Punctuation**
   - `Semicolon` - `;`
   - `Comma` - `,`
   - `Dot` - `.`
   - `Colon` - `:`
   - Order by commonality/importance

7. **Operators**
   - `Equals` - `=`
   - `Plus` - `+`
   - `Minus` - `-`
   - `Star` - `*`
   - `Slash` - `/`
   - Add new operators here, ordered by precedence/commonality

## Token Naming Conventions

- **Keywords**: Use the keyword name in PascalCase (e.g., `Module` for `module`, `Fn` for `fn`)
- **Operators**: Descriptive names (e.g., `Plus` for `+`, `Arrow` for `->`)
- **Punctuation**: Descriptive names (e.g., `Dot` for `.`, `Semicolon` for `;`)
- **Braces**: `L{Name}` and `R{Name}` (e.g., `LParen`/`RParen`, `LBrace`/`RBrace`)

## Adding Tokens

**IMPORTANT**:
- Place tokens in the **correct category**
- Maintain **alphabetical order** within each category (except where precedence matters)
- Add `Eq` and `Hash` derives to the Token enum if not present:
  ```rust
  #[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
  ```

Example - adding a new keyword `import`:
```rust
// ===== Declaration Keywords =====
#[token("fn")]
Fn,

#[token("import")]  // Add alphabetically
Import,

#[token("let")]
Let,

#[token("module")]
Module,
```

## Lexer Testing

Add tests to the `#[cfg(test)] mod tests` section:

```rust
#[test]
fn test_feature_name() {
    let source = "example source code";
    let tokens: Vec<_> = lex(source)
        .filter_map(|t| t.ok())
        .collect();

    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0].value, Token::FeatureName);
    assert_eq!(tokens[1].value, Token::Identifier);
    // ... more assertions
}
```

# Step 3: Create Parser Module

Parsers are organized by feature in `lib/kestrel-parser/src/`.

## Event-Driven Architecture

Kestrel uses an **event-driven parser architecture** inspired by rust-analyzer:

1. **Internal Chumsky parsers** (`*_internal()`) parse tokens and return raw data (spans, tuples)
2. **Event emitters** (`emit_*()`) take raw data and emit parsing events to an EventSink
3. **Public parsers** (`parse_*()`) coordinate: run Chumsky parser → emit events → handle errors
4. **EventSink** collects events: `StartNode`, `AddToken`, `FinishNode`, `Error`
5. **TreeBuilder** converts events into a lossless `SyntaxNode` tree

### Why Event-Driven?

- **Lossless trees**: All source text is preserved (whitespace, comments, etc.)
- **Error recovery**: Parsing can continue after errors
- **IDE-friendly**: Supports incremental parsing and error-tolerant analysis
- **Testable**: Can inspect events before tree building
- **Flexible**: Easy to add error reporting, diagnostics, etc.

### Event Flow

```
Source → Lexer → Tokens → Internal Parser → Raw Data
                              ↓
                         Event Emitter → Events → EventSink
                              ↓
                         TreeBuilder → SyntaxNode (lossless CST)
```

## Directory Structure

For a feature named `{feature}` (e.g., `module`):

```
lib/kestrel-parser/src/
  {feature}/
    mod.rs        # Main declaration struct and parser
    path.rs       # Sub-types if needed (like ModulePath)
    other.rs      # Other feature-specific types
```

## File: `{feature}/path.rs` (if needed)

Create sub-types that are **exclusive to this feature**. These wrap syntax tree nodes:

```rust
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode, SyntaxToken};

use crate::event::{EventSink, TreeBuilder};

/// Represents a {feature} path/component
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct {Feature}Path {
    pub syntax: SyntaxNode,  // Lossless syntax tree
}

impl {Feature}Path {
    /// Create from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax }
    }

    /// Extract segment names from the syntax tree
    pub fn segment_names(&self) -> Vec<String> {
        self.syntax
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .filter(|tok| tok.kind() == SyntaxKind::Identifier)
            .map(|tok| tok.text().to_string())
            .collect()
    }
}
```

**Why use SyntaxNode?**
- Lossless representation preserving all source text
- Enables IDE features (formatting, refactoring, etc.)
- Source locations built into the tree structure
- Can derive any data from the tree without storing duplicates

## File: `{feature}/mod.rs`

Main feature implementation using **event-driven architecture**:

```rust
mod path;  // If you created path.rs

pub use path::{Feature}Path;  // Re-export sub-types

use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};

/// Represents a {feature} declaration
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct {Feature}Declaration {
    pub syntax: SyntaxNode,  // Lossless syntax tree
    pub span: Span,          // Overall span for error reporting
}

impl {Feature}Declaration {
    /// Create from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Access child components via the syntax tree
    pub fn component(&self) -> {Feature}Component {
        self.syntax
            .children()
            .find(|node| node.kind() == SyntaxKind::{Feature}Component)
            .map(|node| {Feature}Component { syntax: node })
            .expect("{Feature}Declaration must have a {Feature}Component child")
    }
}

/// Internal Chumsky parser for {feature} components
/// Returns raw parsed data (spans, not syntax trees)
fn {feature}_component_parser_internal() -> impl Parser<Token, ComponentData, Error = Simple<Token>> + Clone {
    // Parse and return spans/raw data
    // Example: parse identifiers and return their spans
    filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
}

/// Internal Chumsky parser for the full {feature} declaration
/// Returns raw parsed data that will be used to emit events
fn {feature}_declaration_parser_internal() -> impl Parser<Token, (Span, ComponentData), Error = Simple<Token>> + Clone {
    just(Token::{Feature}Keyword)
        .map_with_span(|_, span| span)
        .then({feature}_component_parser_internal())
}

/// Parse a {feature} declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_{feature}_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match {feature}_declaration_parser_internal().parse(stream) {
        Ok((keyword_span, component_data)) => {
            emit_{feature}_declaration(sink, keyword_span, &component_data);
        }
        Err(errors) => {
            // Emit error events for each parse error
            for error in errors {
                sink.error(format!("Parse error: {:?}", error));
            }
        }
    }
}

/// Emit events for a {feature} declaration
/// Internal helper function
fn emit_{feature}_declaration(sink: &mut EventSink, keyword_span: Span, component_data: &ComponentData) {
    sink.start_node(SyntaxKind::{Feature}Declaration);
    sink.add_token(SyntaxKind::{Feature}Keyword, keyword_span);
    // Emit events for components...
    sink.finish_node();
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    #[test]
    fn test_{feature}_basic() {
        let source = "feature example";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_{feature}_declaration(source, tokens.into_iter(), &mut sink);

        // Build tree from events
        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = {Feature}Declaration {
            syntax: tree,
            span: 0..source.len(),
        };

        // Assert on the parsed structure via the syntax tree
        assert_eq!(decl.syntax.kind(), SyntaxKind::{Feature}Declaration);
    }
}
```

## Naming Conventions

- **Structs**: `{Feature}Declaration`, `{Feature}Path`, `{Feature}Type` (PascalCase)
- **Public parser functions**: `parse_{feature}_declaration()`, `parse_{feature}_path()` (snake_case, starts with `parse_`)
- **Internal Chumsky parsers**: `{feature}_parser_internal()` (snake_case, ends with `_internal`)
- **Event emitters**: `emit_{feature}_declaration()` (snake_case, starts with `emit_`, usually private)
- **Test functions**: `test_{feature}_{what}` (snake_case)

## Parser Patterns

### Matching Tokens
```rust
just(Token::Keyword)                    // Match exact token
filter_map(|span, token| ...)           // Match with custom logic
```

### Combinators
```rust
.separated_by(just(Token::Comma))       // Comma-separated
.at_least(1)                             // One or more
.or_not()                                // Optional
.ignore_then(parser)                     // Sequence, ignore first
.then(parser)                            // Sequence, keep both
.map(|x| ...)                            // Transform result
.map_with_span(|x, span| ...)            // Transform with span
```

### Common Patterns
```rust
// Identifier list: A, B, C
filter_map(|span, token| match token {
    Token::Identifier => Ok(span),
    _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
})
.separated_by(just(Token::Comma))
.at_least(1)

// Optional type annotation: : Type
just(Token::Colon)
    .ignore_then(type_parser())
    .or_not()

// Parenthesized list: (A, B, C)
just(Token::LParen)
    .ignore_then(item_parser().separated_by(just(Token::Comma)))
    .then_ignore(just(Token::RParen))
```

# Step 4: Add SyntaxKind Variants

File: `lib/kestrel-syntax-tree/src/lib.rs`

Add your feature's syntax node kinds to the `SyntaxKind` enum:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxKind {
    // ===== Syntax Nodes (Non-terminals) =====
    Root,
    SourceFile,
    DeclarationItem,
    ImportDeclaration,
    {Feature}Declaration,  // Add your feature here
    {Feature}Component,    // Add sub-components
    // ...
}
```

**Important**: Also update the `kind_from_raw` match statement to include your new variants:

```rust
const {FEATURE}_DECLARATION: u16 = SyntaxKind::{Feature}Declaration as u16;
// ... in match statement
{FEATURE}_DECLARATION => SyntaxKind::{Feature}Declaration,
```

# Step 5: Update Main Parser Module

File: `lib/kestrel-parser/src/lib.rs`

Add your feature module and re-export commonly used types:

```rust
pub mod event;
pub mod module;
pub mod {feature};  // Add this
pub mod parser;

// Re-export commonly used types
pub use module::{ModuleDeclaration, ModulePath};
pub use {feature}::{
    {Feature}Declaration,
    {Feature}Path,  // If applicable
    // ... other commonly used items
};

// Re-export event-driven parse functions
pub use module::{parse_module_declaration, parse_module_path};
pub use {feature}::{
    parse_{feature}_declaration,
    // ... other parse functions
};

// Re-export Parser API
pub use parser::{Parser, ParseResult, ParseError};
```

## High-Level Parser API

Users of your feature can use the high-level `Parser` struct:

```rust
use kestrel_parser::{Parser, parse_{feature}_declaration};
use kestrel_lexer::lex;

let source = "feature code here";
let tokens: Vec<_> = lex(source)
    .filter_map(|t| t.ok())
    .map(|spanned| (spanned.value, spanned.span))
    .collect();

// Parse and get result with tree + errors
let result = Parser::parse(source, tokens.into_iter(), parse_{feature}_declaration);

println!("Tree: {:?}", result.tree);
for error in result.errors {
    println!("Error: {}", error.message);
}
```

# Step 6: Create Kestrel Language Test Files

Create integration test files written in Kestrel to test the feature.

## Directory Structure

```
tests/
  {feature}/
    basic.ks           # Basic usage examples
    edge_cases.ks      # Edge cases and corner cases
    errors.ks          # Expected error cases (if applicable)
```

## Test File Conventions

**File**: `tests/{feature}/basic.ks`

Write actual Kestrel code demonstrating the feature:

```kestrel
// Basic usage of {feature}
module Test.Feature

// Example 1: Simple case
{feature example}

// Example 2: Complex case
{more complex example}
```

**File**: `tests/{feature}/edge_cases.ks`

Test boundary conditions and unusual but valid cases:

```kestrel
module Test.Feature.EdgeCases

// Edge case 1: Minimal valid usage
{minimal example}

// Edge case 2: Maximum complexity
{complex example with many parts}

// Edge case 3: Unicode identifiers
{example with café or αβγ}
```

**File**: `tests/{feature}/errors.ks` (optional)

Document expected error cases with comments:

```kestrel
module Test.Feature.Errors

// ERROR: Should fail - missing required component
// {invalid example}

// ERROR: Should fail - wrong syntax
// {another invalid example}
```

## Test File Guidelines

1. **Start with module declaration**: Every test file should begin with `module Test.{Feature}`
2. **Use comments liberally**: Explain what each test demonstrates
3. **Show variety**: Include simple, complex, and edge cases
4. **Be realistic**: Write code that would actually be used
5. **Test interactions**: Show how the feature works with other features
6. **Keep files focused**: Each file should test one aspect (basic usage, edge cases, errors)

## Example: Module Feature

`tests/module/basic.ks`:
```kestrel
// Basic module declarations
module A

// Nested module path
module A.B.C

// Unicode module names
module Café.αβγ.世界
```

`tests/module/edge_cases.ks`:
```kestrel
// Single segment module
module Main

// Very long module path
module A.B.C.D.E.F.G.H.I.J.K
```

# Step 7: Test Everything

Run tests to ensure everything works:

```bash
# Test lexer
cargo test -p kestrel-lexer

# Test parser (includes event system and Parser struct tests)
cargo test -p kestrel-parser

# Test syntax tree
cargo test -p kestrel-syntax-tree

# Test everything
cargo test

# Manually verify test files can be parsed
# Use the Parser API in main.rs:
cargo run
```

## Integration Testing

Your feature should now work with `parse_source_file` for parsing multiple declarations:

```rust
use kestrel_parser::{Parser, parse_source_file};
use kestrel_lexer::lex;

let source = "module Test\n{feature} declaration";
let tokens: Vec<_> = lex(source)
    .filter_map(|t| t.ok())
    .map(|spanned| (spanned.value, spanned.span))
    .collect();

// Parse entire file
let result = Parser::parse(source, tokens.into_iter(), parse_source_file);

// Root is SourceFile with multiple declaration children
assert_eq!(result.tree.kind(), SyntaxKind::SourceFile);
assert_eq!(result.tree.children().count(), 2); // module + feature
```

# What to Reuse vs. Create New

## Reuse These

- **Existing tokens**: If the token already exists (like `Identifier`, `Dot`, `Semicolon`), don't add it again
- **Common patterns**: Use existing helper types from `kestrel_span` (like `Span`, `Spanned`)
- **Testing patterns**: Follow the same test structure as existing tests

## Create New

- **Feature-specific structs**: Create new AST types for your feature (e.g., `ModuleDeclaration`)
- **Sub-types**: Create files like `path.rs` if the feature has complex sub-components
- **Parsers**: Always create new parser functions for your feature
- **Tokens**: Add new tokens for feature-specific keywords/operators
- **Test files**: Always create new `.ks` test files in `tests/{feature}/`

## Don't Duplicate

- Don't add `Identifier`, `Integer`, etc. - these are shared across features
- Don't add common punctuation like `;`, `,`, `.` if they already exist
- Don't create generic utilities - add to `kestrel_span` if needed

# Example: Module Feature

See the implementation of `module` as a reference:
- Tokens: `Module` keyword, `Dot` punctuation (reused `Identifier`)
- Parser: `lib/kestrel-parser/src/module/`
  - `path.rs`: `ModulePath` type
  - `mod.rs`: `ModuleDeclaration` and parsers
- Tests:
  - Rust tests in `lib/kestrel-parser/src/module/mod.rs`
  - Kestrel tests in `tests/module/` directory

# Common Pitfalls

## Lexer Pitfalls
1. **Wrong category**: Putting a declaration keyword in statement keywords
2. **Wrong order**: Not maintaining alphabetical order within categories
3. **Missing derives**: Forgetting `Eq` and `Hash` on Token enum
4. **Creating duplicate tokens**: Adding `Semicolon` when it already exists

## Parser Pitfalls
5. **Not using event-driven API**: Creating parsers that return AST directly instead of emitting events
6. **Wrong naming**: Using `{feature}_parser()` instead of `parse_{feature}_declaration()` for public API
7. **Storing data in AST**: Storing strings/data in structs instead of wrapping `SyntaxNode`
8. **Not emitting all tokens**: Forgetting to emit punctuation/keyword tokens (e.g., dots, commas)
9. **Wrong event order**: Not matching StartNode/FinishNode correctly
10. **Missing error handling**: Not emitting Error events when parsing fails
11. **Wrong test pattern**: Using old parser API instead of event-driven tests

## SyntaxKind Pitfalls
12. **Forgetting SyntaxKind**: Not adding your feature's node types to the SyntaxKind enum
13. **Not updating kind_from_raw**: Adding to enum but forgetting the match statement

## General Pitfalls
14. **Forgetting re-exports**: Not updating `lib.rs` to re-export new types
15. **No test files**: Forgetting to create `.ks` test files in `tests/` directory
16. **Poor test coverage**: Only testing happy path, not edge cases
17. **Not using Parser struct**: Writing manual event collection instead of using `Parser::parse()`

# Checklist

## Lexer
- [ ] Read and understand the feature document
- [ ] Identify all required tokens (keywords, operators, punctuation)
- [ ] Add tokens to lexer in correct categories with correct names
- [ ] Ensure Token enum has `Eq` and `Hash` derives
- [ ] Add lexer tests for the new tokens

## SyntaxKind
- [ ] Add feature node types to `SyntaxKind` enum in `lib/kestrel-syntax-tree/src/lib.rs`
- [ ] Update `kind_from_raw` match statement with new constants

## Parser
- [ ] Create parser module directory `lib/kestrel-parser/src/{feature}/`
- [ ] Create AST wrapper types that wrap `SyntaxNode` (Declaration, Path, etc.)
- [ ] Implement internal Chumsky parsers (`{feature}_parser_internal()`)
- [ ] Implement event emitters (`emit_{feature}_declaration()`)
- [ ] Implement public event-driven parsers (`parse_{feature}_declaration()`)
- [ ] Add parser tests using event-driven API (EventSink + TreeBuilder)
- [ ] Update `lib/kestrel-parser/src/lib.rs`:
  - [ ] Add `pub mod {feature};`
  - [ ] Re-export types: `pub use {feature}::{Feature}Declaration;`
  - [ ] Re-export parsers: `pub use {feature}::parse_{feature}_declaration;`

## Integration with parse_source_file
- [ ] Add your feature to `declaration_item/mod.rs`:
  - [ ] Add internal parser to `declaration_item_parser_internal()`
  - [ ] Add event emission to `parse_source_file()`
- [ ] Test that your feature works with `Parser::parse()` and `parse_source_file()`

## Testing
- [ ] Create `tests/{feature}/` directory
- [ ] Write `tests/{feature}/basic.ks` with common usage examples
- [ ] Write `tests/{feature}/edge_cases.ks` with boundary conditions
- [ ] Write `tests/{feature}/errors.ks` if there are error cases
- [ ] Run `cargo test -p kestrel-lexer` to test lexer
- [ ] Run `cargo test -p kestrel-parser` to test parser
- [ ] Run `cargo test` to test everything
- [ ] Update main.rs to demonstrate parsing your test files
- [ ] Run `cargo run` to verify test files parse correctly

Now implement the feature from the provided document!
