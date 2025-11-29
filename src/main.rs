use kestrel_lexer::lex;
use kestrel_parser::{parse_source_file, Parser};
use kestrel_reporting::DiagnosticContext;
use std::fs;

/// Parse a single file and add it to an existing semantic tree
fn add_file(
    path: &str,
    semantic_tree: &mut kestrel_semantic_tree_builder::SemanticTree,
    diagnostics: &mut DiagnosticContext,
) {
    println!("\n  Adding file: {}", path);

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            println!("    ❌ Error reading file: {}", e);
            return;
        }
    };

    // Lex the entire file
    let tokens: Vec<_> = lex(&content)
        .filter_map(|t| t.ok())
        .map(|spanned| (spanned.value, spanned.span))
        .collect();

    // Parse the entire file using the new Parser API
    let result = Parser::parse(&content, tokens.into_iter(), parse_source_file);

    // Display results
    if !result.errors.is_empty() {
        println!("    ❌ Parse errors ({} errors):", result.errors.len());
        for error in &result.errors {
            println!("       {}", error.message);
        }
    } else {
        println!("    ✓ Parsed successfully!");
    }

    // Add to semantic tree
    let file_id = diagnostics.add_file(path.to_string(), content.clone());
    kestrel_semantic_tree_builder::add_file_to_tree(
        semantic_tree,
        path,
        &result.tree,
        &content,
        diagnostics,
        file_id,
    );
}

/// Process a group of files together (for testing cross-file imports)
fn process_file_group(name: &str, files: &[&str]) {
    println!("\n{}", "=".repeat(70));
    println!("Processing group: {}", name);
    println!("{}", "=".repeat(70));

    let mut semantic_tree = kestrel_semantic_tree_builder::SemanticTree::new();
    let mut diagnostics = DiagnosticContext::new();

    // Add all files to the same semantic tree
    for file in files {
        add_file(file, &mut semantic_tree, &mut diagnostics);
    }

    // Run binding phase to resolve imports
    println!("\n  Running bind phase...");
    kestrel_semantic_tree_builder::bind_tree(&semantic_tree, &mut diagnostics, 0);
    println!("    ✓ Bind phase complete!");

    // Check for diagnostics (validation errors, import errors, etc.)
    if diagnostics.len() > 0 {
        println!("\n--- Diagnostics ---");
        diagnostics.emit().unwrap();
    }

    // Print semantic tree (shows hierarchy)
    println!("\n--- Semantic Tree ---");
    kestrel_semantic_tree_builder::print_semantic_tree(&semantic_tree);

    // Print symbol table
    println!("\n--- Symbol Table ---");
    kestrel_semantic_tree_builder::print_symbol_table(&semantic_tree);

    println!();
}

fn main() {
    // Test basic import resolution
    process_file_group("Basic Import Resolution", &[
        "tests/import_resolution/library.ks",
        "tests/import_resolution/consumer.ks",
        "tests/import_resolution/specific_import.ks",
        "tests/import_resolution/aliased_import.ks",
    ]);

    // Test error cases
    process_file_group("Import Errors", &[
        "tests/import_resolution/library.ks",
        "tests/import_resolution/import_private.ks",
        "tests/import_resolution/import_nonexistent.ks",
        "tests/import_resolution/import_bad_module.ks",
    ]);

    // Test nested modules
    process_file_group("Nested Modules", &[
        "tests/import_resolution/nested/math.ks",
        "tests/import_resolution/nested/math_geometry.ks",
        "tests/import_resolution/nested/math_algebra.ks",
        "tests/import_resolution/nested/consumer.ks",
    ]);

    // Test import conflicts
    process_file_group("Import Conflicts", &[
        "tests/import_resolution/conflict/module_a.ks",
        "tests/import_resolution/conflict/module_b.ks",
        "tests/import_resolution/conflict/consumer_conflict.ks",
        "tests/import_resolution/conflict/consumer_aliased.ks",
        "tests/import_resolution/conflict/consumer_local_conflict.ks",
    ]);

    // Test visibility semantics
    process_file_group("Visibility Semantics", &[
        "tests/import_resolution/visibility_semantics.ks",
    ]);

    // Test cycle detection
    process_file_group("Circular Type Alias Detection", &[
        "tests/cycle_detection/circular_alias.ks",
    ]);

    process_file_group("Self-Referential Type Alias Detection", &[
        "tests/cycle_detection/self_cycle.ks",
    ]);

    // Test struct declarations
    process_file_group("Basic Struct Declarations", &[
        "tests/struct/basic.ks",
    ]);

    process_file_group("Nested Struct Declarations", &[
        "tests/struct/nested.ks",
    ]);

    process_file_group("Struct Fields", &[
        "tests/struct/fields.ks",
    ]);

    process_file_group("Struct Edge Cases", &[
        "tests/struct/edge_cases.ks",
    ]);

    // Test function declarations
    process_file_group("Function Basic Declarations", &[
        "tests/functions/basic.ks",
    ]);

    process_file_group("Function Valid Overloads", &[
        "tests/functions/valid_overloads.ks",
    ]);

    process_file_group("Function Duplicate Errors", &[
        "tests/functions/duplicate_errors.ks",
    ]);

    process_file_group("Function Edge Cases", &[
        "tests/functions/edge_cases.ks",
    ]);

    process_file_group("Functions In Structs", &[
        "tests/functions/in_structs.ks",
    ]);

    println!("\n{}", "=".repeat(70));
    println!("All test groups processed!");
    println!("{}", "=".repeat(70));
}
