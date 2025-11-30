# Creating Validation Passes

This document explains how to create validation passes for the Kestrel compiler. Validation passes run after semantic analysis to check for errors that can't be caught during parsing or binding.

## Overview

Validation passes are located in `lib/kestrel-semantic-tree-builder/src/validation/`. Each pass:

1. Implements the `ValidationPass` trait
2. Walks the semantic tree looking for violations
3. Reports errors via `DiagnosticContext`

## Process for Creating a New Pass

### Step 1: Check for Overlap

Before creating a new pass, check if the validation already exists:

1. **Existing passes** - Review the passes in `validation/mod.rs`:
   - `FunctionBodyPass` - Functions must have bodies (except in protocols)
   - `ProtocolMethodPass` - Protocol methods cannot have bodies
   - `StaticContextPass` - `static` only allowed in struct/protocol
   - `DuplicateSymbolPass` - No duplicate types or members
   - `VisibilityConsistencyPass` - Public APIs can't expose private types

2. **Binding-phase checks** - Some validation happens during binding in `lib.rs`:
   - `check_type_alias_cycles` - Detects circular type aliases
   - `check_duplicate_signatures` - Detects duplicate function signatures

3. **Parser-level checks** - Some errors are caught during parsing

### Step 2: Define the Errors

Before writing code, define exactly what errors the pass will detect. Create a table:

| Condition | Error Message |
|-----------|---------------|
| When X happens | "error message describing X" |
| When Y happens | "error message describing Y" |

**Example for `StaticContextPass`:**

| Condition | Error Message |
|-----------|---------------|
| Static function at module level | "static modifier is only allowed inside struct or protocol" |

**Example for `DuplicateSymbolPass`:**

| Condition | Error Message |
|-----------|---------------|
| Two types with same name | "duplicate type 'X': already defined as {kind}" |
| Two members with same name | "duplicate member 'X' in {kind} 'Y': already defined as {member_kind}" |

### Step 3: Implement the Pass

#### 3.1 Create the file

Create a new file in `lib/kestrel-semantic-tree-builder/src/validation/`:

```rust
//! Validation pass for [description]
//!
//! [Explain what this pass checks]

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::validation::{ValidationConfig, ValidationPass};

pub struct MyValidationPass;

impl MyValidationPass {
    const NAME: &'static str = "my_validation";
}

impl ValidationPass for MyValidationPass {
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

fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    // Check this symbol
    // ...

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, diagnostics, config);
    }
}
```

#### 3.2 Register the pass

Update `validation/mod.rs`:

```rust
mod my_validation;  // Add module declaration

pub use my_validation::MyValidationPass;  // Export the pass

impl ValidationRunner {
    pub fn new() -> Self {
        let passes: Vec<Box<dyn ValidationPass>> = vec![
            // ... existing passes ...
            Box::new(MyValidationPass),  // Add to runner
        ];
        Self { passes }
    }
}
```

#### 3.3 Report errors

Use the diagnostic system to report errors:

```rust
fn report_error(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let name = &symbol.metadata().name().value;
    let span = symbol.metadata().declaration_span().clone();
    let file_id = get_file_id_for_symbol(symbol, diagnostics);

    // Include pass name in debug mode
    let message = if config.debug_mode {
        format!("[{}] error message here", MyValidationPass::NAME)
    } else {
        "error message here".to_string()
    };

    let diagnostic = kestrel_reporting::Diagnostic::error()
        .with_message(message)
        .with_labels(vec![
            kestrel_reporting::Label::primary(file_id, span)
                .with_message("label explaining the error location")
        ]);

    diagnostics.add_diagnostic(diagnostic);
}

// Helper to get file ID
fn get_file_id_for_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &DiagnosticContext,
) -> usize {
    let mut current = symbol.clone();
    loop {
        if current.metadata().kind() == KestrelSymbolKind::SourceFile {
            let file_name = current.metadata().name().value.clone();
            return diagnostics.get_file_id(&file_name).unwrap_or(0);
        }
        match current.metadata().parent() {
            Some(parent) => current = parent,
            None => return 0,
        }
    }
}
```

### Step 4: Add Tests

Add tests in `lib/kestrel-test-suite/tests/validation.rs`:

```rust
mod my_validation {
    use super::*;

    #[test]
    fn valid_case_compiles() {
        Test::new(r#"module Test
            // valid code here
        "#)
        .expect(Compiles);
    }

    #[test]
    fn invalid_case_errors() {
        Test::new(r#"module Test
            // invalid code here
        "#)
        .expect(HasError("expected error substring"));
    }
}
```

Run tests with:
```bash
cargo test -p kestrel-test-suite --test validation
```

## Common Patterns

### Walking the Tree with Context

Pass context down through recursion:

```rust
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
    in_special_context: bool,  // Context passed down
) {
    let kind = symbol.metadata().kind();

    // Update context based on current symbol
    let new_context = in_special_context || kind == KestrelSymbolKind::SomeType;

    // Check this symbol using context
    if should_error(symbol, in_special_context) {
        report_error(symbol, diagnostics, config);
    }

    // Pass updated context to children
    for child in symbol.metadata().children() {
        validate_symbol(&child, diagnostics, config, new_context);
    }
}
```

### Accessing Behaviors

Get behavior data from symbols:

```rust
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;

fn get_some_behavior(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<&SomeBehavior> {
    let behaviors = symbol.metadata().behaviors();
    behaviors
        .iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::SomeBehavior))
        .and_then(|b| b.as_ref().downcast_ref::<SomeBehavior>())
}
```

### Checking Symbol Kinds

```rust
let kind = symbol.metadata().kind();

match kind {
    KestrelSymbolKind::Function => { /* ... */ }
    KestrelSymbolKind::Struct => { /* ... */ }
    KestrelSymbolKind::Protocol => { /* ... */ }
    KestrelSymbolKind::Class => { /* ... */ }
    KestrelSymbolKind::TypeAlias => { /* ... */ }
    KestrelSymbolKind::Field => { /* ... */ }
    KestrelSymbolKind::Module => { /* ... */ }
    KestrelSymbolKind::SourceFile => { /* ... */ }
    _ => {}
}
```

### Collecting Children for Duplicate Detection

```rust
use std::collections::HashMap;

fn check_duplicates(scope: &Arc<dyn Symbol<KestrelLanguage>>) {
    let mut seen: HashMap<String, Arc<dyn Symbol<KestrelLanguage>>> = HashMap::new();

    for child in scope.metadata().children() {
        let name = child.metadata().name().value.clone();

        if let Some(first) = seen.get(&name) {
            // Report duplicate error
        } else {
            seen.insert(name, child.clone());
        }
    }
}
```

## Tips

1. **Skip the root symbol** - The semantic tree root is a placeholder with kind `Class` and name `<root>`. Skip it when checking for type-specific rules.

2. **Use debug mode** - Support `config.debug_mode` to include pass name in errors for debugging.

3. **Test both valid and invalid cases** - Ensure valid code compiles and invalid code produces the expected error.

4. **Check existing passes for patterns** - The existing passes in `validation/` demonstrate common patterns.

5. **Consider the symbol hierarchy** - Understand parent-child relationships:
   - Root > Module > SourceFile > declarations
   - Struct/Class/Protocol > members (fields, functions)
