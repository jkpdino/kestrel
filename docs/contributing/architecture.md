# Kestrel Architecture

## Compilation Pipeline

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           COMPILATION PIPELINE                               │
└─────────────────────────────────────────────────────────────────────────────┘

Source Code ("module Main\nstruct Point { ... }")
       │
       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  PHASE 1: LEXING                                    [kestrel-lexer]         │
│  ────────────────                                                           │
│  Input:  Source string                                                      │
│  Output: Iterator<Spanned<Token>>                                           │
│  Library: Logos                                                             │
│                                                                             │
│  "module" → Token::Module                                                   │
│  "Main"   → Token::Identifier                                               │
│  "struct" → Token::Struct                                                   │
└─────────────────────────────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  PHASE 2: PARSING                                   [kestrel-parser]        │
│  ────────────────                                                           │
│  Input:  Tokens + Source                                                    │
│  Output: Events (StartNode, AddToken, FinishNode)                           │
│  Library: Chumsky                                                           │
│                                                                             │
│  Event-driven architecture:                                                 │
│    1. Internal Chumsky parser returns raw data (spans, tuples)              │
│    2. Emit functions convert data to events                                 │
│    3. Events collected in EventSink                                         │
└─────────────────────────────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  PHASE 3: SYNTAX TREE                               [kestrel-syntax-tree]   │
│  ────────────────────                                                       │
│  Input:  Events + Source                                                    │
│  Output: SyntaxNode (lossless CST)                                          │
│  Library: Rowan                                                             │
│                                                                             │
│  TreeBuilder converts events → GreenNode → SyntaxNode                       │
│  Preserves all source text (whitespace, comments, trivia)                   │
└─────────────────────────────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  PHASE 4: SEMANTIC ANALYSIS                                                 │
│  ──────────────────────────                                                 │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │  4a. BUILD                           [kestrel-semantic-tree-builder]│    │
│  │  ─────────                                                          │    │
│  │  Resolvers extract symbols from syntax nodes                        │    │
│  │  Creates: ModuleSymbol, StructSymbol, FunctionSymbol, etc.          │    │
│  │  Attaches: Behaviors (Visibility, Callable, Typed)                  │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│                              │                                              │
│                              ▼                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │  4b. BIND                            [kestrel-semantic-tree-builder]│    │
│  │  ────────                                                           │    │
│  │  Resolves type references to concrete types                         │    │
│  │  Validates imports, detects cycles                                  │    │
│  │  Body resolution for expressions/statements                         │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│                              │                                              │
│                              ▼                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │  4c. VALIDATE                        [kestrel-semantic-tree-builder]│    │
│  │  ───────────                                                        │    │
│  │  Validation passes check semantic constraints:                      │    │
│  │  - FunctionBodyPass: functions need bodies (except protocols)       │    │
│  │  - ProtocolMethodPass: protocol methods can't have bodies           │    │
│  │  - StaticContextPass: static only in struct/protocol                │    │
│  │  - DuplicateSymbolPass: no duplicate types/members                  │    │
│  │  - VisibilityConsistencyPass: public APIs consistency               │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  OUTPUT                                             [kestrel-compiler]      │
│  ──────                                                                     │
│  Compilation {                                                              │
│      semantic_tree: SemanticTree,   // Symbol hierarchy                     │
│      diagnostics: Vec<Diagnostic>,  // Errors and warnings                  │
│  }                                                                          │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Crate Dependencies

```
                    ┌──────────────────┐
                    │ kestrel-compiler │  ← High-level API
                    └────────┬─────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
        ▼                    ▼                    ▼
┌───────────────┐  ┌─────────────────────┐  ┌─────────────────┐
│kestrel-parser │  │kestrel-semantic-tree│  │kestrel-reporting│
└───────┬───────┘  │      -builder       │  └─────────────────┘
        │          └──────────┬──────────┘
        │                     │
        │          ┌──────────┴──────────┐
        │          │                     │
        ▼          ▼                     ▼
┌───────────────┐  ┌────────────────┐  ┌──────────────┐
│kestrel-syntax │  │kestrel-semantic│  │kestrel-prelude│
│    -tree      │  │    -tree       │  └──────────────┘
└───────┬───────┘  └───────┬────────┘
        │                  │
        │          ┌───────┴───────┐
        ▼          ▼               ▼
┌───────────────┐  ┌──────────────┐  ┌─────────────┐
│ kestrel-lexer │  │semantic-tree │  │kestrel-span │
└───────────────┘  └──────────────┘  └─────────────┘
```

## Key Types by Phase

### Phase 1: Lexing
```rust
// kestrel-lexer
Token              // Enum: Identifier, Module, Struct, LBrace, ...
Spanned<Token>     // Token + Span (Range<usize>)

// Usage
let tokens: Vec<Spanned<Token>> = lex(source).filter_map(|t| t.ok()).collect();
```

### Phase 2: Parsing
```rust
// kestrel-parser
Event              // StartNode(SyntaxKind), AddToken(kind, span), FinishNode, Error
EventSink          // Collects events during parsing
TreeBuilder        // Converts events to SyntaxNode

// Pattern: internal parser → emit → public parse function
fn foo_parser_internal() -> impl Parser<Token, RawData, ...>
fn emit_foo(sink: &mut EventSink, data: RawData)
pub fn parse_foo(source: &str, tokens: I, sink: &mut EventSink)
```

### Phase 3: Syntax Tree
```rust
// kestrel-syntax-tree
SyntaxKind         // Enum: tokens (Identifier, Module) + nodes (StructDeclaration, Name)
SyntaxNode         // Rowan node with children
SyntaxToken        // Rowan token with text

// Tree structure
StructDeclaration
├── Visibility      // Wrapper (may be empty)
│   └── Public      // Token (optional)
├── Struct          // Token
├── Name            // Wrapper (always has content)
│   └── Identifier  // Token
└── StructBody
    └── ...
```

### Phase 4: Semantic Analysis
```rust
// kestrel-semantic-tree
Symbol<KestrelLanguage>     // Trait for all symbols
SymbolMetadata              // Name, span, children, behaviors
KestrelSymbolKind           // Module, Struct, Function, Field, ...

// Specific symbols
ModuleSymbol, StructSymbol, FunctionSymbol, FieldSymbol,
ProtocolSymbol, TypeAliasSymbol, ImportSymbol, LocalSymbol

// Behaviors (attached to symbols)
VisibilityBehavior          // Access control
CallableBehavior            // Function signatures
TypedBehavior               // Type information
ExecutableBehavior          // Code bodies

// kestrel-semantic-tree-builder
Resolver                    // Trait: builds symbol from syntax
ResolverRegistry            // Maps SyntaxKind → Resolver
BodyResolver                // Resolves expressions/statements
ValidationPass              // Trait: checks semantic constraints
```

## File Organization

```
lib/kestrel-lexer/
└── src/lib.rs              # Single file: Token enum + lex()

lib/kestrel-parser/
└── src/
    ├── lib.rs              # Re-exports
    ├── event.rs            # Event, EventSink, TreeBuilder
    ├── parser.rs           # High-level Parser API
    ├── declaration_item/   # Top-level declarations
    │   └── mod.rs
    ├── module/             # Module-specific parsing
    │   └── mod.rs
    ├── struct/             # Struct-specific parsing
    │   └── mod.rs
    ├── function/           # Function-specific parsing
    │   └── mod.rs
    ├── expr/               # Expression parsing
    │   └── mod.rs
    └── common/             # Shared parser utilities
        ├── data.rs
        ├── emitters.rs
        └── parsers.rs

lib/kestrel-syntax-tree/
└── src/lib.rs              # SyntaxKind enum + KestrelLanguage

lib/kestrel-semantic-tree/
└── src/
    ├── lib.rs              # Re-exports
    ├── language.rs         # KestrelLanguage definition
    ├── symbol/
    │   ├── mod.rs
    │   ├── kind.rs         # KestrelSymbolKind enum
    │   ├── module.rs       # ModuleSymbol
    │   ├── struct.rs       # StructSymbol
    │   ├── function.rs     # FunctionSymbol
    │   └── ...
    ├── behavior/
    │   ├── mod.rs
    │   ├── visibility.rs
    │   ├── callable.rs
    │   └── ...
    ├── ty/                 # Type system
    │   └── mod.rs
    ├── expr.rs             # Expression semantics
    └── stmt.rs             # Statement semantics

lib/kestrel-semantic-tree-builder/
└── src/
    ├── lib.rs              # Public API: add_file_to_tree, bind_tree, run_validation
    ├── resolver.rs         # ResolverRegistry
    ├── resolvers/
    │   ├── mod.rs
    │   ├── module.rs
    │   ├── struct.rs
    │   ├── function.rs
    │   └── ...
    ├── body_resolver.rs    # Expression/statement resolution
    ├── type_resolver.rs    # Type path resolution
    ├── path_resolver.rs    # Name path resolution
    ├── local_scope.rs      # Scope management
    ├── validation/
    │   ├── mod.rs          # ValidationRunner
    │   ├── function_body.rs
    │   ├── protocol_method.rs
    │   └── ...
    └── diagnostics/
        ├── mod.rs
        ├── call.rs         # Function call errors
        ├── member_access.rs
        └── ...

lib/kestrel-test-suite/
└── src/lib.rs              # Test fluent API
└── tests/
    ├── body_resolution.rs  # Expression/statement tests
    ├── functions.rs
    ├── structs.rs
    ├── protocols.rs
    └── ...
```

## Data Flow Example

Adding `5.toString()` (primitive method call):

```
1. LEXER
   "5.toString()" → [Integer(5), Dot, Identifier(toString), LParen, RParen]

2. PARSER
   Internal parser extracts: receiver_span, dot_span, method_span, args_spans
   Emitter produces events:
     StartNode(MethodCallExpr)
       AddToken(Integer, 0..1)
       AddToken(Dot, 1..2)
       AddToken(Identifier, 2..10)
       AddToken(LParen, 10..11)
       AddToken(RParen, 11..12)
     FinishNode

3. SYNTAX TREE
   MethodCallExpr
   ├── Integer "5"
   ├── Dot "."
   ├── Identifier "toString"
   ├── LParen "("
   └── RParen ")"

4. SEMANTIC ANALYSIS (body_resolver.rs)
   - Recognize Integer literal → Expr::Integer(5)
   - Look up "toString" method on Int type from prelude
   - Resolve to: Expr::MethodCall { receiver: Int, method: toString, args: [] }
   - Return type: String
```
