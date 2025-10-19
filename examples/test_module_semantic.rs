use kestrel_lexer::lex;
use kestrel_parser::{parse_source_file, Parser};
use kestrel_semantic_tree_builder::{build_semantic_tree, print_semantic_tree, print_symbol_table};
use std::fs;

fn main() {
    println!("=== Module System Test ===\n");

    // Read the test file
    let source = fs::read_to_string("examples/test_module_system.ks")
        .expect("Failed to read test file");

    println!("Source code:");
    println!("{}", source);
    println!();

    // Lex the source
    let tokens: Vec<_> = lex(&source)
        .filter_map(|t| t.ok())
        .map(|spanned| (spanned.value, spanned.span))
        .collect();

    // Parse into syntax tree
    let result = Parser::parse(&source, tokens.into_iter(), parse_source_file);

    if !result.errors.is_empty() {
        println!("Parse errors:");
        for error in &result.errors {
            println!("  {}", error.message);
        }
        return;
    }

    println!("Syntax tree parsed successfully\n");

    // Build semantic tree
    let semantic_tree = build_semantic_tree(&result.tree, &source);

    println!("=== Semantic Tree (Symbol Hierarchy) ===\n");
    print_semantic_tree(&semantic_tree);

    println!("\n=== Symbol Table ===\n");
    print_symbol_table(&semantic_tree);

    println!("\n=== Test Complete ===");
}
