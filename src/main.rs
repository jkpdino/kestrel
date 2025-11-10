use kestrel_lexer::lex;
use kestrel_parser::{parse_source_file, Parser};
use kestrel_reporting::DiagnosticContext;
use std::fs;

fn parse_file(path: &str) {
    println!("\n{}", "=".repeat(70));
    println!("Processing file: {}", path);
    println!("{}", "=".repeat(70));

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            println!("❌ Error reading file: {}", e);
            return;
        }
    };

    // Remove comments and empty lines for display purposes
    let lines: Vec<&str> = content
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty() && !l.starts_with("//"))
        .collect();

    println!("\nParsing file with {} lines\n", lines.len());

    // Lex the entire file
    let tokens: Vec<_> = lex(&content)
        .filter_map(|t| t.ok())
        .map(|spanned| (spanned.value, spanned.span))
        .collect();

    // Parse the entire file using the new Parser API
    let result = Parser::parse(&content, tokens.into_iter(), parse_source_file);

    // Display results
    if !result.errors.is_empty() {
        println!("❌ Parse errors ({} errors):", result.errors.len());
        for error in &result.errors {
            println!("   {}", error.message);
        }
        println!();
    } else {
        println!("✓ Parsed successfully!");
        println!();
    }

    // Build semantic tree
    let mut semantic_tree = kestrel_semantic_tree_builder::SemanticTree::new();
    let mut diagnostics = DiagnosticContext::new();
    let file_id = diagnostics.add_file(path.to_string(), content.clone());
    kestrel_semantic_tree_builder::add_file_to_tree(&mut semantic_tree, path, &result.tree, &content, &mut diagnostics, file_id);

    // Check for module validation errors
    if diagnostics.len() > 0 {
        println!("\n--- Module Validation Errors ---");
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
    let test_files = vec![
        //"tests/declaration_item/basic.ks",
        //"tests/module/basic.ks",
        //"tests/import/basic.ks",
        //"tests/class/basic.ks",
        //"tests/class/edge_cases.ks",
        "tests/mixed.ks",
        "tests/class/nested.ks",
        "tests/type_alias/basic.ks",
        "tests/type_alias/mixed_features.ks",
    ];

    for file in test_files {
        parse_file(file);
    }

    println!("\n{}", "=".repeat(70));
    println!("All test files processed!");
    println!("{}", "=".repeat(70));
}
