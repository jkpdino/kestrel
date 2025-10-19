---
description: Complete end-to-end implementation of a Kestrel language feature including lexer, parser, syntax tree, semantic symbols, and resolvers. Takes a feature document path and implements the full 7-step pipeline.
---

You are implementing a complete language feature for the Kestrel compiler. You will read a feature specification document and implement the entire pipeline from lexer tokens through to semantic symbols.

# Architecture Overview

Kestrel uses a **two-phase compilation architecture**:

## Phase 1: Syntax Layer (Concrete Syntax Tree)
- **Lexer** (`kestrel-lexer`): Tokenizes source code using Logos
- **Parser** (`kestrel-parser`): Event-driven parsers using Chumsky + event emission
- **Syntax Tree** (`kestrel-syntax-tree`): Lossless CST using Rowan library
- **Output**: `SyntaxNode` tree preserving all source text

## Phase 2: Semantic Layer (Semantic Tree)
- **Semantic Tree** (`semantic-tree`): Language-agnostic symbol framework
- **Kestrel Semantic Tree** (`kestrel-semantic-tree`): Kestrel-specific symbols and behaviors
- **Semantic Tree Builder** (`kestrel-semantic-tree-builder`): Resolvers that build semantic symbols from syntax
- **Output**: Symbol hierarchy with semantic information

## Compilation Pipeline

```
Source Code
    ↓
[Lexer] → Tokens
    ↓
[Parser] → Events → TreeBuilder → SyntaxNode (CST)
    ↓
[Semantic Builder] → Resolvers → Symbol Tree
    ↓
Type Checking / Analysis
```

# Step 1: Understand the Feature

Read the provided feature document carefully. Identify:
- **Keywords**: New reserved words (e.g., `class`, `fn`, `if`)
- **Literals**: New literal types (strings, numbers, etc.)
- **Operators**: New operators or punctuation (e.g., `->`, `::`, `..`)
- **Syntax structures**: How these tokens combine (e.g., `class Foo { ... }`)
- **Semantic meaning**: What symbols/behaviors the feature introduces
- **Visibility/behaviors**: Does it support visibility modifiers? Other behaviors?

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
   - Keywords that declare things: `class`, `fn`, `let`, `module`, etc.
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

7. **Operators**
   - `Equals` - `=`
   - `Plus` - `+`
   - `Minus` - `-`
   - `Star` - `*`
   - `Slash` - `/`

## Token Naming Conventions

- **Keywords**: Use the keyword name in PascalCase (e.g., `Class` for `class`, `Fn` for `fn`)
- **Operators**: Descriptive names (e.g., `Plus` for `+`, `Arrow` for `->`)
- **Punctuation**: Descriptive names (e.g., `Dot` for `.`, `Semicolon` for `;`)
- **Braces**: `L{Name}` and `R{Name}` (e.g., `LParen`/`RParen`)

## Example: Adding Class Keyword

```rust
// ===== Declaration Keywords =====
#[token("class")]
Class,

#[token("fn")]
Fn,

#[token("import")]
Import,
```

## Lexer Testing

Add tests to verify token recognition:

```rust
#[test]
fn test_class_declaration() {
    let source = "class Foo { }";
    let tokens: Vec<_> = lex(source)
        .filter_map(|t| t.ok())
        .collect();

    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[0].value, Token::Class);
    assert_eq!(tokens[1].value, Token::Identifier);
    assert_eq!(tokens[2].value, Token::LBrace);
    assert_eq!(tokens[3].value, Token::RBrace);
}
```

# Step 3: Add SyntaxKind Variants

File: `lib/kestrel-syntax-tree/src/lib.rs`

Add your feature's syntax node kinds to the `SyntaxKind` enum:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxKind {
    // ===== Syntax Nodes (Non-terminals) =====
    Root,
    SourceFile,
    DeclarationItem,
    ClassDeclaration,    // Add main node type
    ClassBody,           // Add child node types
    Name,
    Visibility,
    // ...
}
```

Also update the `From<Token>` implementation:

```rust
impl From<Token> for SyntaxKind {
    fn from(token: Token) -> Self {
        match token {
            Token::Class => SyntaxKind::Class,
            // ... other mappings
        }
    }
}
```

**Important**: Update the `kind_from_raw` match statement:

```rust
impl Language for KestrelLanguage {
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        const CLASS_DECLARATION: u16 = SyntaxKind::ClassDeclaration as u16;
        const CLASS_BODY: u16 = SyntaxKind::ClassBody as u16;

        match raw.0 {
            CLASS_DECLARATION => SyntaxKind::ClassDeclaration,
            CLASS_BODY => SyntaxKind::ClassBody,
            // ... other cases
        }
    }
}
```

# Step 4: Create Parser Module

Parsers are organized by feature in `lib/kestrel-parser/src/`.

## Directory Structure

```
lib/kestrel-parser/src/
  {feature}/
    mod.rs        # Main declaration struct and parser
```

## File: `{feature}/mod.rs`

Create event-driven parser implementation following the Class pattern:

```rust
use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};

/// Represents a {feature} declaration
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct {Feature}Declaration {
    pub syntax: SyntaxNode,  // Lossless syntax tree
    pub span: Span,
}

impl {Feature}Declaration {
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the feature name from this declaration
    pub fn name(&self) -> Option<String> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::Name)?
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tok| tok.kind() == SyntaxKind::Identifier)
            .map(|tok| tok.text().to_string())
    }

    /// Get the visibility modifier if present
    pub fn visibility(&self) -> Option<SyntaxKind> {
        let visibility_node = self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::Visibility)?;

        visibility_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tok| {
                matches!(
                    tok.kind(),
                    SyntaxKind::Public | SyntaxKind::Private |
                    SyntaxKind::Internal | SyntaxKind::Fileprivate
                )
            })
            .map(|tok| tok.kind())
    }
}

/// Internal Chumsky parser for optional visibility modifier
fn visibility_parser_internal() -> impl Parser<Token, Option<(Token, Span)>, Error = Simple<Token>> + Clone {
    filter_map(|span, token| match token {
        Token::Public | Token::Private | Token::Internal | Token::Fileprivate => {
            Ok((token, span))
        }
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .or_not()
}

/// Internal Chumsky parser for {feature} declaration
/// Returns: (visibility, keyword_span, name_span, ...)
fn {feature}_declaration_parser_internal() -> impl Parser<
    Token,
    (Option<(Token, Span)>, Span, Span, /* ... */),
    Error = Simple<Token>,
> + Clone {
    visibility_parser_internal()
        .then(just(Token::{Feature}).map_with_span(|_, span| span))
        .then(filter_map(|span, token| match token {
            Token::Identifier => Ok(span),
            _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
        }))
        // Add more parsing logic here
        .map(|((visibility, keyword_span), name_span)| {
            (visibility, keyword_span, name_span)
        })
}

/// Parse a {feature} declaration and emit events
pub fn parse_{feature}_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match {feature}_declaration_parser_internal().parse(stream) {
        Ok((visibility, keyword_span, name_span)) => {
            emit_{feature}_declaration(sink, visibility, keyword_span, name_span);
        }
        Err(errors) => {
            for error in errors {
                sink.error(format!("Parse error: {:?}", error));
            }
        }
    }
}

/// Emit events for a {feature} declaration
fn emit_{feature}_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    keyword_span: Span,
    name_span: Span,
) {
    sink.start_node(SyntaxKind::{Feature}Declaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
    if let Some((vis_token, vis_span)) = visibility {
        let vis_kind = match vis_token {
            Token::Public => SyntaxKind::Public,
            Token::Private => SyntaxKind::Private,
            Token::Internal => SyntaxKind::Internal,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            _ => unreachable!(),
        };
        sink.add_token(vis_kind, vis_span);
    }
    sink.finish_node(); // Finish Visibility

    sink.add_token(SyntaxKind::{Feature}, keyword_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    sink.finish_node(); // Finish {Feature}Declaration
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    #[test]
    fn test_{feature}_basic() {
        let source = "{feature} Foo";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_{feature}_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = {Feature}Declaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Foo".to_string()));
        assert_eq!(decl.visibility(), None);
    }
}
```

## Update Main Parser Module

File: `lib/kestrel-parser/src/lib.rs`

```rust
pub mod {feature};

pub use {feature}::{
    {Feature}Declaration,
    parse_{feature}_declaration,
};
```

## Integrate into Declaration Items

File: `lib/kestrel-parser/src/declaration_item/mod.rs`

Add to the `DeclarationItem` enum:

```rust
use crate::{feature}::{Feature}Declaration, parse_{feature}_declaration};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarationItem {
    Module(ModuleDeclaration),
    Import(ImportDeclaration),
    Class(ClassDeclaration),
    {Feature}({Feature}Declaration),  // Add this
}
```

Add to internal `DeclarationItemData`:

```rust
enum DeclarationItemData {
    Module(Span, Vec<Span>),
    Import(/* ... */),
    Class(/* ... */),
    {Feature}(/* parsed data */),  // Add this
}
```

Update the parser to recognize your feature and emit events.

# Step 5: Create Semantic Symbol

Now we transition from syntax to semantics. Symbols represent the semantic meaning of declarations.

## Add Symbol Kind

File: `lib/kestrel-semantic-tree/src/symbol/kind.rs`

```rust
#[derive(Copy, Clone, Debug)]
pub enum KestrelSymbolKind {
    Class,
    {Feature},  // Add your feature
}
```

## Create Symbol Implementation

File: `lib/kestrel-semantic-tree/src/symbol/{feature}.rs` (create new file)

```rust
use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::visibility::VisibilityBehavior,
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind
};

#[derive(Debug)]
pub struct {Feature}Symbol {
    metadata: SymbolMetadata<KestrelLanguage>,
}

impl Symbol<KestrelLanguage> for {Feature}Symbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl {Feature}Symbol {
    /// Create a new {Feature}Symbol with a name, span, and visibility
    pub fn new(name: Name, span: Span, visibility: VisibilityBehavior) -> Self {
        let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::{Feature})
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility))
            .build();

        {Feature}Symbol { metadata }
    }
}
```

**Key Points:**
- Symbol wraps `SymbolMetadata<KestrelLanguage>`
- Implements `Symbol<KestrelLanguage>` trait
- Constructor takes semantic info: name, span, behaviors
- Uses `SymbolMetadataBuilder` to construct metadata

## Update Module Exports

File: `lib/kestrel-semantic-tree/src/symbol/mod.rs`

```rust
pub mod class;
pub mod {feature};  // Add this
pub mod kind;

pub use class::ClassSymbol;
pub use {feature}::{Feature}Symbol;  // Add this
pub use kind::KestrelSymbolKind;
```

File: `lib/kestrel-semantic-tree/src/lib.rs`

```rust
pub mod behavior;
pub mod language;
pub mod symbol;

pub use symbol::{ClassSymbol, {Feature}Symbol, KestrelSymbolKind};  // Add {Feature}Symbol
```

# Step 6: Create Resolver

Resolvers bridge the syntax and semantic layers by extracting semantic information from syntax trees and creating symbols.

## Create Resolver Implementation

File: `lib/kestrel-semantic-tree-builder/src/resolvers/{feature}.rs` (create new file)

```rust
use std::sync::Arc;

use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::{feature}::{Feature}Symbol;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::Resolver;
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope,
    get_node_span, get_visibility_span, parse_visibility,
};

/// Resolver for {feature} declarations
pub struct {Feature}Resolver;

impl Resolver for {Feature}Resolver {
    fn build_declaration(
        &self,
        syntax: &SyntaxNode,
        source: &str,
        parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        // 1. Extract name from Name node
        let name_str = extract_name(syntax)?;
        let name_node = find_child(syntax, SyntaxKind::Name)?;
        let name_span = get_node_span(&name_node, source);
        let name = Spanned::new(name_str, name_span.clone());

        // 2. Get full span of the declaration
        let full_span = get_node_span(syntax, source);

        // 3. Extract visibility modifier
        let visibility_str = extract_visibility(syntax);
        let visibility_enum = visibility_str.as_deref().and_then(parse_visibility);
        let visibility_span = get_visibility_span(syntax, source).unwrap_or(name_span);

        // 4. Determine visibility scope (what can access this)
        let visibility_scope = find_visibility_scope(visibility_enum.as_ref(), parent, root);

        // 5. Create visibility behavior
        let visibility_behavior = VisibilityBehavior::new(
            visibility_enum,
            visibility_span,
            visibility_scope,
        );

        // 6. Create the {feature} symbol
        let symbol = {Feature}Symbol::new(name, full_span, visibility_behavior);
        let symbol_arc: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(symbol);

        // 7. Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&symbol_arc);
        }

        Some(symbol_arc)
    }
}
```

**Key Utilities (all available in `utils.rs`):**
- `extract_name(syntax)` - Get name string from Name node
- `extract_visibility(syntax)` - Get visibility string from Visibility node
- `parse_visibility(str)` - Convert string to `Visibility` enum
- `find_child(syntax, kind)` - Find child node with specific kind
- `get_node_span(node, source)` - Get span of a node
- `get_visibility_span(syntax, source)` - Get span of visibility modifier
- `find_visibility_scope(vis, parent, root)` - Determine what can access this symbol

## Update Resolver Module

File: `lib/kestrel-semantic-tree-builder/src/resolvers/mod.rs`

```rust
mod class;
mod {feature};  // Add this
mod import;
mod module;
mod terminal;

pub use class::ClassResolver;
pub use {feature}::{Feature}Resolver;  // Add this
pub use import::ImportResolver;
pub use module::ModuleResolver;
pub use terminal::TerminalResolver;
```

# Step 7: Register Resolver

File: `lib/kestrel-semantic-tree-builder/src/resolver.rs`

Add your resolver to the registry:

```rust
use crate::resolvers::{
    ClassResolver, {Feature}Resolver, ImportResolver,
    ModuleResolver, TerminalResolver
};

impl ResolverRegistry {
    pub fn new() -> Self {
        let mut resolvers: HashMap<SyntaxKind, Box<dyn Resolver>> = HashMap::new();

        // Register declaration resolvers
        resolvers.insert(
            SyntaxKind::ClassDeclaration,
            Box::new(ClassResolver),
        );
        resolvers.insert(
            SyntaxKind::{Feature}Declaration,  // Add this
            Box::new({Feature}Resolver),        // Add this
        );
        resolvers.insert(
            SyntaxKind::ModuleDeclaration,
            Box::new(ModuleResolver),
        );
        // ... other resolvers

        ResolverRegistry { resolvers }
    }
}
```

Now your feature is fully integrated! The semantic tree builder will:
1. Walk the syntax tree
2. Encounter `{Feature}Declaration` nodes
3. Look up `{Feature}Resolver` in the registry
4. Call `build_declaration()` to create `{Feature}Symbol`
5. Add the symbol to the semantic tree hierarchy

# Step 8: Create Test Files

Create comprehensive test files in the `tests/{feature}/` directory that test your feature in realistic scenarios.

## What to Test

### 1. Basic Standalone Usage
Start with simple, isolated examples of your feature working on its own.

### 2. Visibility Modifiers
If your feature supports visibility (`public`, `private`, `internal`, `fileprivate`), test each modifier.

### 3. Feature Interactions (CRITICAL)
Test how your feature works WITH other features:
- If your feature can contain other declarations (like classes can), test it containing functions, classes, modules, etc.
- If your feature can be nested inside other declarations, test it inside classes, modules, etc.
- Create realistic files mixing your feature with imports, classes, functions, modules
- Example: Functions should be tested inside classes, classes should contain functions, etc.

### 4. Nesting (if applicable)
If your feature supports nesting:
- Single-level nesting
- Deep nesting (3-5 levels)
- Multiple siblings
- Mixing visibility modifiers in nested structures

### 5. Edge Cases
- Unicode identifiers (café, 世界, αβγ, Привет)
- Very long identifiers
- Single-character identifiers
- Maximum nesting depth
- Many siblings (10+ declarations)
- Whitespace variations

### 6. Error Cases
Document invalid syntax in comments with `// ERROR:` prefix explaining what should fail.

## Test File Principles

**Progressive Complexity**: Start simple, build up to realistic complexity
```kestrel
// Simple case
{feature} Basic

// Add visibility
public {feature} WithVisibility

// Add nesting
public {feature} WithNesting {
  {feature} Nested
}

// Realistic scenario mixing multiple features
public {feature} Realistic {
  class Helper {}
  fn utility() {}
  private {feature} Internal
}
```

**Real-World Scenarios**: Write code developers would actually use, not just minimal examples

**Feature Composition**: Show your feature working alongside imports, modules, classes, functions in the same file

**Comments**: Explain what each test demonstrates and why it matters

## Example Test File Structure

```kestrel
// Basic usage
{feature} Simple

// All visibility modifiers
public {feature} Public
private {feature} Private
internal {feature} Internal

// Nesting
{feature} Outer {
  {feature} Inner
}

// Mixed with other features
class Container {
  {feature} Nested
  fn method() {}
}

{feature} WithFunctions {
  fn helper() {}
  class NestedClass {}
}

// Edge cases
{feature} 世界
{feature} VeryLongIdentifierName

// ERROR: Invalid cases (commented out)
// {feature} missing-identifier
// {feature} 123InvalidStart
```

# Step 9: Run Tests

```bash
# Test lexer
cargo test -p kestrel-lexer

# Test parser
cargo test -p kestrel-parser

# Test syntax tree
cargo test -p kestrel-syntax-tree

# Test semantic tree
cargo test -p kestrel-semantic-tree

# Test semantic tree builder
cargo test -p kestrel-semantic-tree-builder

# Test everything
cargo test
```

# Common Patterns and Best Practices

## Parser Patterns

### Optional Elements
```rust
just(Token::Colon)
    .ignore_then(type_parser())
    .or_not()
```

### Lists
```rust
item_parser()
    .separated_by(just(Token::Comma))
    .at_least(1)
```

### Sequences
```rust
just(Token::Fn)
    .then(identifier_parser())
    .then(parameter_list_parser())
    .then(body_parser())
```

### Recursive Structures
```rust
recursive(|nested_parser| {
    simple_parser().or(nested_parser)
})
```

## Event Emission Patterns

### Always Wrap in Nodes
```rust
sink.start_node(SyntaxKind::{Feature}Declaration);
// Emit tokens and child nodes
sink.finish_node();
```

### Name Node Pattern
```rust
sink.start_node(SyntaxKind::Name);
sink.add_token(SyntaxKind::Identifier, name_span);
sink.finish_node();
```

### Visibility Node Pattern
```rust
sink.start_node(SyntaxKind::Visibility);
if let Some((vis_token, vis_span)) = visibility {
    let vis_kind = match vis_token {
        Token::Public => SyntaxKind::Public,
        Token::Private => SyntaxKind::Private,
        Token::Internal => SyntaxKind::Internal,
        Token::Fileprivate => SyntaxKind::Fileprivate,
        _ => unreachable!(),
    };
    sink.add_token(vis_kind, vis_span);
}
sink.finish_node();
```

## Symbol Creation Pattern

```rust
let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::{Feature})
    .with_name(name.clone())
    .with_declaration_span(name.span.clone())
    .with_span(full_span)
    .with_behavior(Arc::new(visibility_behavior))
    .build();

{Feature}Symbol { metadata }
```

## Resolver Pattern

```rust
// 1. Extract data from syntax tree
let name_str = extract_name(syntax)?;
let name_span = /* ... */;
let name = Spanned::new(name_str, name_span);

// 2. Extract behaviors (visibility, mutability, etc.)
let visibility = /* ... */;

// 3. Create symbol
let symbol = {Feature}Symbol::new(name, span, visibility);
let symbol_arc: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(symbol);

// 4. Add to parent
if let Some(parent) = parent {
    parent.metadata().add_child(&symbol_arc);
}

Some(symbol_arc)
```

# Example: Complete Class Feature

See the Class implementation as a reference:

## Lexer
- Token: `Class` at line 94
- Visibility tokens: `Public`, `Private`, `Internal`, `Fileprivate`

## Syntax Tree
- `SyntaxKind::ClassDeclaration`
- `SyntaxKind::ClassBody`
- `SyntaxKind::Visibility`
- `SyntaxKind::Name`

## Parser
- File: `lib/kestrel-parser/src/class/mod.rs` (599 LOC)
- `ClassDeclaration` struct with `name()`, `visibility()`, `children()` methods
- `parse_class_declaration()` public API
- `emit_class_declaration()` event emitter
- Supports nested classes

## Semantic Symbol
- File: `lib/kestrel-semantic-tree/src/symbol/class.rs` (31 LOC)
- `ClassSymbol` wrapping `SymbolMetadata<KestrelLanguage>`
- Constructor takes `name`, `span`, `visibility`

## Resolver
- File: `lib/kestrel-semantic-tree-builder/src/resolvers/class.rs` (63 LOC)
- `ClassResolver` implementing `Resolver` trait
- Extracts name, visibility, and span from syntax
- Creates `ClassSymbol` and adds to parent

## Registration
- Added to `ResolverRegistry` in `resolver.rs:53-56`

# Complete Checklist

## Lexer (Step 2)
- [ ] Add tokens to `Token` enum in correct categories
- [ ] Maintain alphabetical order within categories
- [ ] Add lexer tests for new tokens
- [ ] Verify `Token` enum has `Eq` and `Hash` derives

## Syntax Tree (Step 3)
- [ ] Add `SyntaxKind` variants for all node types
- [ ] Update `From<Token>` implementation
- [ ] Update `kind_from_raw()` with new constants and match arms

## Parser (Step 4)
- [ ] Create `lib/kestrel-parser/src/{feature}/mod.rs`
- [ ] Create `{Feature}Declaration` struct wrapping `SyntaxNode`
- [ ] Implement internal Chumsky parsers (`*_internal()`)
- [ ] Implement event emitters (`emit_*()`)
- [ ] Implement public parser (`parse_{feature}_declaration()`)
- [ ] Add parser tests using `EventSink` + `TreeBuilder`
- [ ] Update `lib/kestrel-parser/src/lib.rs`:
  - [ ] Add `pub mod {feature};`
  - [ ] Re-export types: `pub use {feature}::{Feature}Declaration;`
  - [ ] Re-export parsers: `pub use {feature}::parse_{feature}_declaration;`
- [ ] Add to `declaration_item/mod.rs`:
  - [ ] Add to `DeclarationItem` enum
  - [ ] Add to `DeclarationItemData` enum
  - [ ] **CRITICAL: Add internal Chumsky parser to `declaration_item_parser_internal()`**
    - Create a `{feature}_parser` variable following the same pattern as `class_parser` or `fn_parser`
    - Add it to the `.or()` chain at the end: `module_parser.or(import_parser).or(class_parser).or({feature}_parser)`
    - Without this, your feature will NOT show up in the semantic tree (0 top-level symbols)
  - [ ] Add event emission in `parse_source_file()` match statement
  - [ ] Add event emission in `emit_declaration_item_internal()` helper function

## Semantic Symbol (Step 5)
- [ ] Add to `KestrelSymbolKind` enum in `symbol/kind.rs`
- [ ] Create `lib/kestrel-semantic-tree/src/symbol/{feature}.rs`
- [ ] Implement `{Feature}Symbol` struct
- [ ] Implement `Symbol<KestrelLanguage>` trait
- [ ] Create constructor with name, span, and behaviors
- [ ] Update `symbol/mod.rs` to export new symbol
- [ ] Update `lib/kestrel-semantic-tree/src/lib.rs` to re-export

## Resolver (Step 6)
- [ ] Create `lib/kestrel-semantic-tree-builder/src/resolvers/{feature}.rs`
- [ ] Implement `{Feature}Resolver` struct
- [ ] Implement `Resolver::build_declaration()`:
  - [ ] Extract name using `extract_name()`
  - [ ] Extract visibility using `extract_visibility()`
  - [ ] Get spans using `get_node_span()`
  - [ ] Create visibility behavior
  - [ ] Create symbol
  - [ ] Add to parent
- [ ] Update `resolvers/mod.rs` to export resolver

## Registration (Step 7)
- [ ] Add resolver to `ResolverRegistry::new()` in `resolver.rs`
- [ ] Map `SyntaxKind::{Feature}Declaration` to `{Feature}Resolver`

## Testing (Step 8-9)
- [ ] Create `tests/{feature}/` directory
- [ ] Write `basic.ks` with common usage
- [ ] Write `edge_cases.ks` with boundary conditions
- [ ] Write `nested.ks` if feature supports nesting
- [ ] Run `cargo test -p kestrel-lexer`
- [ ] Run `cargo test -p kestrel-parser`
- [ ] Run `cargo test -p kestrel-syntax-tree`
- [ ] Run `cargo test -p kestrel-semantic-tree`
- [ ] Run `cargo test -p kestrel-semantic-tree-builder`
- [ ] Run `cargo test` (all tests)

## Verification
- [ ] Lexer recognizes all new tokens
- [ ] Parser creates correct syntax tree structure
- [ ] Syntax tree preserves all source information
- [ ] Resolver creates semantic symbols from syntax
- [ ] Symbols have correct parent-child relationships
- [ ] Visibility behavior works correctly
- [ ] All tests pass
- [ ] Test files parse successfully

# Understanding Wrapper Node Patterns

Kestrel uses **wrapper nodes** for semantic elements that need uniform extraction. This is a key architectural pattern you must understand.

## The Wrapper Node Pattern

**Principle**: Any piece of syntax that needs to be extracted uniformly across different declaration types should be wrapped in a dedicated node.

### Pattern 1: Name Node (Identifier Wrapper)

**Every identifier that names something** must be wrapped in a `Name` node:

```
{Feature}Declaration
  ├─ Name                    ← Wrapper node (SyntaxKind::Name)
  │   └─ Identifier "Foo"    ← Actual identifier token
```

**Why wrap?**
- Single extraction point: `extract_name()` works on all declarations
- Refactoring support: Find all "Name" nodes for rename operations
- Future extensions: Could add type parameters, annotations, etc. to Name node

**How to implement:**

Parser (emit):
```rust
sink.start_node(SyntaxKind::Name);          // Start wrapper
sink.add_token(SyntaxKind::Identifier, name_span);  // Wrapped content
sink.finish_node();                          // Finish wrapper
```

Utility (extract):
```rust
/// Extract name from a Name node
pub fn extract_name(syntax: &SyntaxNode) -> Option<String> {
    // 1. Find the Name wrapper node
    let name_node = find_child(syntax, SyntaxKind::Name)?;

    // 2. Extract the Identifier token inside it
    name_node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tok| tok.kind() == SyntaxKind::Identifier)
        .map(|tok| tok.text().to_string())
}
```

Resolver (use):
```rust
let name_str = extract_name(syntax)?;  // "Foo"
```

### Pattern 2: Visibility Node (Optional Content Wrapper)

**Every declaration** must have a `Visibility` node that may or may not contain a visibility token:

```
{Feature}Declaration
  ├─ Visibility              ← Wrapper node (always present)
  │   └─ Public "public"     ← Token (only if specified)
```

Or when no modifier:
```
{Feature}Declaration
  ├─ Visibility              ← Wrapper node (empty, no children)
```

**Why always emit even if empty?**
- Uniform structure: All declarations have Visibility node
- No special cases: `extract_visibility()` always looks in same place
- Optional modifiers: Wrapper exists whether content present or not

**How to implement:**

Parser (emit):
```rust
// ALWAYS emit Visibility wrapper
sink.start_node(SyntaxKind::Visibility);

// Conditionally emit content
if let Some((vis_token, vis_span)) = visibility {
    let vis_kind = match vis_token {
        Token::Public => SyntaxKind::Public,
        Token::Private => SyntaxKind::Private,
        Token::Internal => SyntaxKind::Internal,
        Token::Fileprivate => SyntaxKind::Fileprivate,
        _ => unreachable!(),
    };
    sink.add_token(vis_kind, vis_span);
}

sink.finish_node();  // Finish wrapper (may be empty)
```

Utility (extract):
```rust
/// Extract visibility modifier from a node with a Visibility child
pub fn extract_visibility(syntax: &SyntaxNode) -> Option<String> {
    // 1. Find the Visibility wrapper node
    let visibility_node = find_child(syntax, SyntaxKind::Visibility)?;

    // 2. Try to find a visibility token inside (may not exist)
    let visibility_token = visibility_node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .next()?;  // Returns None if wrapper is empty

    // 3. Convert token kind to string
    let vis_text = match visibility_token.kind() {
        SyntaxKind::Public => "public",
        SyntaxKind::Private => "private",
        SyntaxKind::Internal => "internal",
        SyntaxKind::Fileprivate => "fileprivate",
        _ => return None,
    };

    Some(vis_text.to_string())
}
```

Resolver (use):
```rust
let visibility_str = extract_visibility(syntax);  // Option<String>
// Some("public") or None if no modifier
```

## When to Create a Wrapper Node

Create a wrapper node when:

1. **Multiple declaration types need the same info**
   - Example: Classes, functions, structs all have names → `Name` wrapper
   - Example: Classes, functions, variables can be public/private → `Visibility` wrapper

2. **The syntax element is optional**
   - Example: Type annotations `fn foo() -> Int` vs `fn bar()` → `ReturnType` wrapper
   - Wrapper exists in both cases, may be empty

3. **You want uniform extraction**
   - Need `extract_{something}()` to work the same everywhere
   - Prevents duplicating extraction logic

4. **Future extensions are likely**
   - Example: `Name` might later hold generic parameters: `Name<T>`
   - Wrapper makes it easy to extend

## When NOT to Create a Wrapper Node

Don't create a wrapper when:

1. **Only one declaration type uses it**
   - Example: Class-specific body syntax → directly emit as `ClassBody`

2. **It's already a token**
   - Example: Keywords like `class`, `fn` → emit directly as `SyntaxKind::Class`

3. **It's a list or complex structure**
   - Example: Parameter lists → create `ParameterList` node, not a wrapper

## Standard Declaration Structure

Every declaration should follow this pattern:

```
{Feature}Declaration
  ├─ Visibility (wrapper, may be empty)
  │   └─ Public|Private|etc. (optional token)
  ├─ {Feature}Keyword (token)
  ├─ Name (wrapper, always has content)
  │   └─ Identifier (token)
  └─ (feature-specific nodes...)
```

## Creating New Wrapper Patterns

If you need a new wrapper (e.g., `TypeAnnotation`, `Attributes`, `Modifiers`):

1. **Add SyntaxKind variants:**
   ```rust
   pub enum SyntaxKind {
       TypeAnnotation,    // Wrapper
       TypeIdentifier,    // Content inside wrapper
   }
   ```

2. **Emit in parser:**
   ```rust
   sink.start_node(SyntaxKind::TypeAnnotation);
   if let Some(type_span) = type_annotation {
       sink.add_token(SyntaxKind::TypeIdentifier, type_span);
   }
   sink.finish_node();  // May be empty if no type annotation
   ```

3. **Create utility in `utils.rs`:**
   ```rust
   /// Extract type annotation from a TypeAnnotation wrapper node
   pub fn extract_type_annotation(syntax: &SyntaxNode) -> Option<String> {
       let type_node = find_child(syntax, SyntaxKind::TypeAnnotation)?;
       type_node
           .children_with_tokens()
           .filter_map(|elem| elem.into_token())
           .find(|tok| tok.kind() == SyntaxKind::TypeIdentifier)
           .map(|tok| tok.text().to_string())
   }
   ```

4. **Use in resolvers:**
   ```rust
   let type_str = extract_type_annotation(syntax);  // Option<String>
   ```

## Extraction Utilities Reference

Current utilities in `lib/kestrel-semantic-tree-builder/src/utils.rs`:

| Function | Wrapper Node | Content | Returns |
|----------|--------------|---------|---------|
| `extract_name(syntax)` | `Name` | `Identifier` | `Option<String>` |
| `extract_visibility(syntax)` | `Visibility` | `Public\|Private\|etc.` | `Option<String>` |
| `parse_visibility(str)` | (converts string) | - | `Option<Visibility>` enum |
| `get_visibility_span(syntax, source)` | `Visibility` | (span of token inside) | `Option<Span>` |

Supporting functions:
- `find_child(syntax, kind)` - Find wrapper node
- `get_node_span(node, source)` - Get span of any node

## Example: Full Resolver Using Wrappers

```rust
impl Resolver for {Feature}Resolver {
    fn build_declaration(
        &self,
        syntax: &SyntaxNode,
        source: &str,
        parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        // Extract from Name wrapper
        let name_str = extract_name(syntax)?;
        let name_node = find_child(syntax, SyntaxKind::Name)?;
        let name_span = get_node_span(&name_node, source);
        let name = Spanned::new(name_str, name_span.clone());

        // Extract from Visibility wrapper
        let visibility_str = extract_visibility(syntax);
        let visibility_enum = visibility_str.as_deref().and_then(parse_visibility);
        let visibility_span = get_visibility_span(syntax, source).unwrap_or(name_span);

        // Create symbol...
    }
}
```

# Common Pitfalls

1. **Forgetting to update `kind_from_raw()`** - Adding `SyntaxKind` but not the match statement
2. **Wrong event order** - Not matching `start_node()`/`finish_node()` pairs
3. **Missing Name wrapper** - Emitting `Identifier` directly instead of wrapping in `Name` node
4. **Missing Visibility node** - Not emitting `Visibility` node when no modifier present (must always emit it, even if empty)
5. **Storing data in structs** - Storing strings instead of wrapping `SyntaxNode`
6. **Not adding to registry** - Creating resolver but forgetting to register it
7. **Wrong symbol kind** - Using wrong `KestrelSymbolKind` variant
8. **Missing parent-child link** - Creating symbol but not adding to parent
9. **Wrong visibility token** - Using `SyntaxKind::Visibility` for the token instead of `SyntaxKind::Public`, etc.
10. **Creating wrapper when not needed** - Adding unnecessary wrappers for single-use syntax

Now implement the complete feature from the provided document!
