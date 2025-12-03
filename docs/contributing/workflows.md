# Common Workflows

Step-by-step guides for common development tasks.

## Adding a New Language Feature

This is the most common workflow. Use this when adding new syntax like a keyword, declaration type, or expression.

### Files Changed (typical)

Based on git history analysis of features like `self` parameter and member access:

| Phase | Files | Lines Changed |
|-------|-------|---------------|
| Lexer | 1 file | ~5-10 |
| Parser | 3-5 files | ~100-300 |
| Syntax Tree | 1 file | ~20-50 |
| Semantic Tree | 2-4 files | ~50-200 |
| Semantic Builder | 2-5 files | ~100-500 |
| Tests | 1-2 files | ~50-200 |

### Step-by-Step

#### 1. Add Token (if new keyword)
**File**: `lib/kestrel-lexer/src/lib.rs`

```rust
// Add in correct category (alphabetical within category)
// Declaration Keywords section:
#[token("newkeyword")]
NewKeyword,
```

#### 2. Add SyntaxKind Variants
**File**: `lib/kestrel-syntax-tree/src/lib.rs`

```rust
pub enum SyntaxKind {
    // Token (if added above)
    NewKeyword,

    // Syntax nodes
    NewFeatureDeclaration,
    NewFeatureBody,  // if applicable
}
```

Update `kind_from_raw`:
```rust
const NEW_FEATURE_DECLARATION: u16 = SyntaxKind::NewFeatureDeclaration as u16;

match raw.0 {
    NEW_FEATURE_DECLARATION => SyntaxKind::NewFeatureDeclaration,
    // ...
}
```

#### 3. Create Parser
**File**: `lib/kestrel-parser/src/{feature}/mod.rs` (new file)

Follow the event-driven parser pattern:
1. Internal Chumsky parser
2. Emit function
3. Public parse function

**File**: `lib/kestrel-parser/src/lib.rs`
```rust
pub mod newfeature;
pub use newfeature::{NewFeatureDeclaration, parse_newfeature_declaration};
```

#### 4. Integrate into Declaration Items
**File**: `lib/kestrel-parser/src/declaration_item/mod.rs`

**Critical**: Add to `declaration_item_parser_internal()` - without this, the feature won't parse!

```rust
let newfeature_parser = /* ... */
    .map(|data| DeclarationItemData::NewFeature(data));

// Add to the .or() chain
module_parser.or(import_parser).or(struct_parser).or(newfeature_parser)
```

#### 5. Add Symbol Kind
**File**: `lib/kestrel-semantic-tree/src/symbol/kind.rs`
```rust
pub enum KestrelSymbolKind {
    NewFeature,
}
```

#### 6. Create Symbol
**File**: `lib/kestrel-semantic-tree/src/symbol/newfeature.rs` (new file)

```rust
pub struct NewFeatureSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
}

impl Symbol<KestrelLanguage> for NewFeatureSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}
```

Update `lib/kestrel-semantic-tree/src/symbol/mod.rs`:
```rust
mod newfeature;
pub use newfeature::NewFeatureSymbol;
```

#### 7. Create Resolver
**File**: `lib/kestrel-semantic-tree-builder/src/resolvers/newfeature.rs` (new file)

Implement the `Resolver` trait.

Update `lib/kestrel-semantic-tree-builder/src/resolvers/mod.rs`:
```rust
mod newfeature;
pub use newfeature::NewFeatureResolver;
```

#### 8. Register Resolver
**File**: `lib/kestrel-semantic-tree-builder/src/resolver.rs`

```rust
resolvers.insert(
    SyntaxKind::NewFeatureDeclaration,
    Box::new(NewFeatureResolver),
);
```

#### 9. Add Tests
**File**: `lib/kestrel-test-suite/tests/newfeature.rs` (new file)

```rust
use kestrel_test_suite::{Test, Compiles, HasError, Symbol, SymbolKind};

#[test]
fn basic_newfeature() {
    Test::new("module Main\nnewfeature Foo { }")
        .expect(Compiles)
        .expect(Symbol::new("Foo").is(SymbolKind::NewFeature));
}
```

#### 10. Verify
```bash
cargo test -p kestrel-lexer
cargo test -p kestrel-parser
cargo test -p kestrel-semantic-tree-builder
cargo test -p kestrel-test-suite
cargo test
```

---

## Adding Expression/Statement Support

When adding new expression or statement types (e.g., binary operators, if expressions).

### Key Files
- `lib/kestrel-parser/src/expr/mod.rs` - Expression parsing
- `lib/kestrel-parser/src/stmt/mod.rs` - Statement parsing
- `lib/kestrel-semantic-tree/src/expr.rs` - Expression semantics
- `lib/kestrel-semantic-tree/src/stmt.rs` - Statement semantics
- `lib/kestrel-semantic-tree-builder/src/body_resolver.rs` - **Main file**

### Step-by-Step

#### 1. Add SyntaxKind
```rust
// In kestrel-syntax-tree/src/lib.rs
BinaryExpr,  // or IfExpr, WhileStmt, etc.
```

#### 2. Update Parser
Add parsing logic in `expr/mod.rs` or `stmt/mod.rs`.

#### 3. Add Semantic Representation
```rust
// In kestrel-semantic-tree/src/expr.rs
pub enum Expr {
    Binary { left: Box<Expr>, op: BinaryOp, right: Box<Expr> },
    // ...
}
```

#### 4. Update Body Resolver
**File**: `lib/kestrel-semantic-tree-builder/src/body_resolver.rs`

This is where most of the work happens. Add a match arm to handle the new syntax:

```rust
fn resolve_expr(&mut self, node: &SyntaxNode) -> Option<Expr> {
    match node.kind() {
        SyntaxKind::BinaryExpr => self.resolve_binary_expr(node),
        // ...
    }
}

fn resolve_binary_expr(&mut self, node: &SyntaxNode) -> Option<Expr> {
    // Extract operands and operator
    // Resolve types
    // Return Expr::Binary { ... }
}
```

#### 5. Add Diagnostics (if needed)
**File**: `lib/kestrel-semantic-tree-builder/src/diagnostics/{name}.rs`

Create a new diagnostics module for feature-specific errors.

#### 6. Add Tests
**File**: `lib/kestrel-test-suite/tests/body_resolution.rs`

```rust
mod binary_ops {
    #[test]
    fn add_integers() {
        Test::new("module Main\nfunc f() -> Int { 1 + 2 }")
            .expect(Compiles);
    }
}
```

---

## Adding a Validation Pass

When adding semantic checks that run after binding (e.g., checking for invalid modifiers).

### Key Files
- `lib/kestrel-semantic-tree-builder/src/validation/{name}.rs` (new)
- `lib/kestrel-semantic-tree-builder/src/validation/mod.rs`
- `lib/kestrel-test-suite/tests/validation.rs`

### Step-by-Step

#### 1. Define Errors
Document what errors the pass will detect:

| Condition | Error Message |
|-----------|---------------|
| When X | "error: X happened" |

#### 2. Create Pass File
**File**: `lib/kestrel-semantic-tree-builder/src/validation/mycheck.rs`

```rust
pub struct MyCheckPass;

impl MyCheckPass {
    const NAME: &'static str = "my_check";
}

impl ValidationPass for MyCheckPass {
    fn name(&self) -> &'static str {
        Self::NAME
    }

    fn validate(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        _db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        config: &ValidationConfig,
    ) {
        validate_symbol(root, diagnostics, config);
    }
}

fn validate_symbol(/* ... */) {
    // Check symbol
    // Recurse into children
}
```

#### 3. Register Pass
**File**: `lib/kestrel-semantic-tree-builder/src/validation/mod.rs`

```rust
mod mycheck;
pub use mycheck::MyCheckPass;

impl ValidationRunner {
    pub fn new() -> Self {
        let passes: Vec<Box<dyn ValidationPass>> = vec![
            // ... existing passes
            Box::new(MyCheckPass),
        ];
        Self { passes }
    }
}
```

#### 4. Add Tests
**File**: `lib/kestrel-test-suite/tests/validation.rs`

```rust
mod my_check {
    #[test]
    fn valid_case() {
        Test::new("module Main\n/* valid code */")
            .expect(Compiles);
    }

    #[test]
    fn invalid_case() {
        Test::new("module Main\n/* invalid code */")
            .expect(HasError("expected error message"));
    }
}
```

---

## Adding a New Diagnostic

When adding error messages for semantic analysis.

### Key Files
- `lib/kestrel-semantic-tree-builder/src/diagnostics/{name}.rs` (new)
- `lib/kestrel-semantic-tree-builder/src/diagnostics/mod.rs`

### Step-by-Step

#### 1. Create Diagnostic Module
**File**: `lib/kestrel-semantic-tree-builder/src/diagnostics/myerror.rs`

```rust
use kestrel_reporting::{Diagnostic, DiagnosticContext, Label};
use kestrel_span::Span;

pub fn report_my_error(
    diagnostics: &mut DiagnosticContext,
    file_id: usize,
    span: Span,
    name: &str,
) {
    let diagnostic = Diagnostic::error()
        .with_message(format!("my error: '{}'", name))
        .with_labels(vec![
            Label::primary(file_id, span)
                .with_message("error occurred here")
        ]);

    diagnostics.add_diagnostic(diagnostic);
}
```

#### 2. Export
**File**: `lib/kestrel-semantic-tree-builder/src/diagnostics/mod.rs`

```rust
mod myerror;
pub use myerror::report_my_error;
```

#### 3. Use in Body Resolver or Validation
```rust
use crate::diagnostics::report_my_error;

// When error condition is detected:
report_my_error(diagnostics, file_id, span, name);
```

---

## Debugging Semantic Resolution Issues

When symbols aren't being created or resolved correctly.

### Diagnostic Steps

#### 1. Check Parser Output
Add debug output to see if syntax tree is correct:
```rust
// In parser test
let tree = TreeBuilder::new(source, sink.into_events()).build();
println!("{:#?}", tree);
```

#### 2. Check Resolver Registration
Verify the resolver is registered in `resolver.rs`:
```rust
// Should see your SyntaxKind mapped to your Resolver
resolvers.insert(SyntaxKind::YourDeclaration, Box::new(YourResolver));
```

#### 3. Check Symbol Creation
Add debug output in your resolver:
```rust
fn build_declaration(&self, syntax: &SyntaxNode, ...) -> Option<...> {
    println!("Resolving: {:?}", syntax.kind());
    // ...
    println!("Created symbol: {:?}", symbol.metadata().name());
}
```

#### 4. Check Parent-Child Links
```rust
if let Some(parent) = parent {
    parent.metadata().add_child(&symbol_arc);
    println!("Added to parent: {:?}", parent.metadata().name());
}
```

#### 5. Use Test Expectations
```rust
Test::new("module Main\nyour code")
    .expect(Compiles)
    .expect(Symbol::new("YourSymbol").is(SymbolKind::YourKind));
```

If the test fails, it shows what symbols actually exist.

---

## Git Workflow

Based on observed commit patterns in this repo:

### Commit Messages
```
feature: description of feature
fix: description of bug fix
refactor: description of refactoring
docs: description of documentation change
test: description of test addition
```

### Feature Commits
Features are typically done in a single commit including:
- Lexer changes
- Parser changes
- Syntax tree changes
- Semantic tree changes
- Builder changes
- Tests

### Running Before Commit
```bash
cargo fmt
cargo clippy
cargo test
```
