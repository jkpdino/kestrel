use clap::{Parser, Subcommand};
use kestrel_lexer::lex;
use kestrel_parser::{parse_source_file, Parser as KestrelParser};
use kestrel_reporting::DiagnosticContext;
use std::fs;
use std::process::ExitCode;

#[derive(Parser)]
#[command(name = "kestrel")]
#[command(about = "The Kestrel compiler", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Source files to process
    #[arg(global = true)]
    files: Vec<String>,

    /// Show semantic tree after analysis
    #[arg(long, global = true)]
    tree: bool,

    /// Show symbol table after analysis
    #[arg(long, global = true)]
    symbols: bool,

    /// Verbose output
    #[arg(short, long, global = true)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Type-check source files
    Check {
        /// Source files to check
        files: Vec<String>,
    },
    /// Parse source files and show syntax tree
    Parse {
        /// Source files to parse
        files: Vec<String>,
    },
}

/// Parse a single file and add it to an existing semantic tree
fn add_file(
    path: &str,
    semantic_tree: &mut kestrel_semantic_tree_builder::SemanticTree,
    diagnostics: &mut DiagnosticContext,
    verbose: bool,
) -> bool {
    if verbose {
        eprintln!("  Parsing {}", path);
    }

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("error: cannot read '{}': {}", path, e);
            return false;
        }
    };

    // Lex the entire file
    let tokens: Vec<_> = lex(&content)
        .filter_map(|t| t.ok())
        .map(|spanned| (spanned.value, spanned.span))
        .collect();

    // Parse the entire file
    let result = KestrelParser::parse(&content, tokens.into_iter(), parse_source_file);

    if !result.errors.is_empty() {
        for error in &result.errors {
            let span = error.span.clone().unwrap_or(0..1);
            let file_id = diagnostics.add_file(path.to_string(), content.clone());
            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(&error.message)
                .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)]);
            diagnostics.add_diagnostic(diagnostic);
        }
        return false;
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

    true
}

fn run_check(files: &[String], show_tree: bool, show_symbols: bool, verbose: bool) -> ExitCode {
    if files.is_empty() {
        eprintln!("error: no input files");
        return ExitCode::from(1);
    }

    let mut semantic_tree = kestrel_semantic_tree_builder::SemanticTree::new();
    let mut diagnostics = DiagnosticContext::new();
    let mut parse_ok = true;

    // Parse all files
    for file in files {
        if !add_file(file, &mut semantic_tree, &mut diagnostics, verbose) {
            parse_ok = false;
        }
    }

    // Run binding phase
    if verbose {
        eprintln!("  Running semantic analysis...");
    }
    kestrel_semantic_tree_builder::bind_tree(&semantic_tree, &mut diagnostics, 0);

    // Show results
    if show_tree {
        println!("--- Semantic Tree ---");
        kestrel_semantic_tree_builder::print_semantic_tree(&semantic_tree);
        println!();
    }

    if show_symbols {
        println!("--- Symbol Table ---");
        kestrel_semantic_tree_builder::print_symbol_table(&semantic_tree);
        println!();
    }

    // Emit diagnostics
    let has_errors = diagnostics.len() > 0 || !parse_ok;
    if has_errors {
        diagnostics.emit().ok();
        ExitCode::from(1)
    } else {
        if verbose {
            eprintln!("  No errors found.");
        }
        ExitCode::SUCCESS
    }
}

fn run_parse(files: &[String], show_tree: bool) -> ExitCode {
    if files.is_empty() {
        eprintln!("error: no input files");
        return ExitCode::from(1);
    }

    let mut has_errors = false;

    for file in files {
        let content = match fs::read_to_string(file) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("error: cannot read '{}': {}", file, e);
                has_errors = true;
                continue;
            }
        };

        let tokens: Vec<_> = lex(&content)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let result = KestrelParser::parse(&content, tokens.into_iter(), parse_source_file);

        println!("=== {} ===", file);

        if !result.errors.is_empty() {
            has_errors = true;
            for error in &result.errors {
                println!("error: {}", error.message);
            }
        } else {
            println!("Parsed successfully.");
        }

        if show_tree {
            println!("\n{:#?}", result.tree);
        }

        println!();
    }

    if has_errors {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Check { files }) => {
            let all_files: Vec<_> = files.iter().chain(cli.files.iter()).cloned().collect();
            run_check(&all_files, cli.tree, cli.symbols, cli.verbose)
        }
        Some(Commands::Parse { files }) => {
            let all_files: Vec<_> = files.iter().chain(cli.files.iter()).cloned().collect();
            run_parse(&all_files, cli.tree)
        }
        None => {
            // Default: check files if provided
            if cli.files.is_empty() {
                eprintln!("error: no input files");
                eprintln!("Run 'kestrel --help' for usage.");
                ExitCode::from(1)
            } else {
                run_check(&cli.files, cli.tree, cli.symbols, cli.verbose)
            }
        }
    }
}
