use kestrel_lexer::lex;
use kestrel_parser::{Parser, parse_source_file};
use kestrel_syntax_tree::SyntaxNode;
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

    println!("\nParsing file with {} non-comment lines\n", lines.len());

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

    println!("--- Syntax Tree ---");
    println!("Root: {:?}", result.tree.kind());
    println!("Children: {} declarations\n", result.tree.children().count());

    // Show each declaration
    for (i, child) in result.tree.children().enumerate() {
        println!("{}", "-".repeat(70));
        println!("Declaration #{}: {:?}", i + 1, child.kind());
        println!("{}", "-".repeat(70));
        print_compact_tree(&child, &content, 0);
        println!();
    }
}

fn print_compact_tree(node: &SyntaxNode, source: &str, indent: usize) {
    let indent_str = "  ".repeat(indent);

    // Print current node
    for element in node.children_with_tokens() {
        if let Some(child_node) = element.as_node() {
            println!("{}└─ {:?}", indent_str, child_node.kind());
            print_compact_tree(child_node, source, indent + 1);
        } else if let Some(token) = element.as_token() {
            let text = token.text();
            println!("{}└─ {:?} '{}'", indent_str, token.kind(), text);
        }
    }
}

fn main() {
    let test_files = vec![
        "tests/declaration_item/basic.ks",
        "tests/module/basic.ks",
        "tests/import/basic.ks",
    ];

    for file in test_files {
        parse_file(file);
    }

    println!("\n{}", "=".repeat(70));
    println!("All test files processed!");
    println!("{}", "=".repeat(70));
}
