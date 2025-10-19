use kestrel_lexer::lex;
use kestrel_parser::{parse_source_file, Parser};
use kestrel_semantic_tree_builder::build_semantic_tree;
use std::fs;

fn main() {
    println!("=== Testing No Module Declaration Error ===\n");
    println!("This test should panic with an error message about missing module declaration.\n");

    // Read the test file (without module declaration)
    let source = fs::read_to_string("examples/test_no_module.ks")
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
    println!("Now attempting to build semantic tree (should panic with error)...\n");

    // This will panic with a descriptive error message
    build_semantic_tree(&result.tree, &source);

    // This line should never be reached
    println!("ERROR: No panic occurred! This is unexpected.");
}
