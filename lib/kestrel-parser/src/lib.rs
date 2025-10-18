//! Kestrel Parser
//!
//! This crate provides event-driven parsing functionality for the Kestrel language.
//! All parsers emit events that are then converted into syntax trees using the `rowan` library
//! via `kestrel-syntax-tree`.
//!
//! # Event-Driven Architecture
//!
//! Parsers in this crate follow an event-driven architecture:
//! 1. Parsers take an `EventSink` and emit parsing events (StartNode, AddToken, FinishNode, Error)
//! 2. Events are collected in the sink
//! 3. A `TreeBuilder` converts events into a concrete syntax tree
//!
//! # Example
//!
//! ```no_run
//! use kestrel_parser::{parse_module_declaration_from_source, event::EventSink};
//! use kestrel_lexer::lex;
//!
//! let source = "module A.B.C";
//! let tokens: Vec<_> = lex(source)
//!     .filter_map(|t| t.ok())
//!     .map(|spanned| (spanned.value, spanned.span))
//!     .collect();
//!
//! // High-level convenience function
//! let decl = parse_module_declaration_from_source(source, tokens.into_iter());
//!
//! // Access the syntax tree
//! println!("Syntax tree: {:?}", decl.syntax);
//!
//! // Access the parsed data via the syntax tree
//! let path = decl.path();
//! println!("Module path: {:?}", path.segment_names());
//! ```
//!
//! # Low-Level Event-Driven API
//!
//! For more control, you can use the event-driven API directly:
//!
//! ```no_run
//! use kestrel_parser::{module::parse_module_declaration, event::{EventSink, TreeBuilder}};
//! use kestrel_parser::module::ModuleDeclaration;
//! use kestrel_lexer::lex;
//!
//! let source = "module A.B.C";
//! let tokens: Vec<_> = lex(source)
//!     .filter_map(|t| t.ok())
//!     .map(|spanned| (spanned.value, spanned.span))
//!     .collect();
//!
//! let mut sink = EventSink::new();
//! parse_module_declaration(source, tokens.into_iter(), &mut sink);
//!
//! // Build tree from events
//! let tree = TreeBuilder::new(source, sink.into_events()).build();
//! let decl = ModuleDeclaration { syntax: tree, span: 0..source.len() };
//! ```

pub mod event;
pub mod module;
pub mod import;
pub mod class;
pub mod declaration_item;
pub mod parser;

use kestrel_lexer::Token;
use kestrel_span::Span;
use event::{EventSink, TreeBuilder};

// Re-export commonly used types
pub use module::{ModuleDeclaration, ModulePath};
pub use import::ImportDeclaration;
pub use class::ClassDeclaration;
pub use declaration_item::DeclarationItem;

// Re-export event-driven parse functions
pub use module::{parse_module_declaration, parse_module_path};
pub use import::parse_import_declaration;
pub use class::parse_class_declaration;
pub use declaration_item::{parse_declaration_item, parse_source_file};

// Re-export Parser API
pub use parser::{Parser, ParseResult, ParseError};

/// Convenience function to parse a module declaration from source and tokens
/// Returns a fully built ModuleDeclaration with its syntax tree
pub fn parse_module_declaration_from_source<I>(source: &str, tokens: I) -> ModuleDeclaration
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let mut sink = EventSink::new();
    module::parse_module_declaration(source, tokens, &mut sink);
    let tree = TreeBuilder::new(source, sink.into_events()).build();
    ModuleDeclaration {
        syntax: tree,
        span: 0..source.len(),
    }
}

/// Convenience function to parse a module path from source and tokens
/// Returns a fully built ModulePath with its syntax tree
pub fn parse_module_path_from_source<I>(source: &str, tokens: I) -> ModulePath
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let mut sink = EventSink::new();
    module::parse_module_path(source, tokens, &mut sink);
    let tree = TreeBuilder::new(source, sink.into_events()).build();
    ModulePath { syntax: tree }
}

/// Convenience function to parse an import declaration from source and tokens
/// Returns a fully built ImportDeclaration with its syntax tree
pub fn parse_import_declaration_from_source<I>(source: &str, tokens: I) -> ImportDeclaration
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let mut sink = EventSink::new();
    import::parse_import_declaration(source, tokens, &mut sink);
    let tree = TreeBuilder::new(source, sink.into_events()).build();
    ImportDeclaration {
        syntax: tree,
        span: 0..source.len(),
    }
}

/// Convenience function to parse a class declaration from source and tokens
/// Returns a fully built ClassDeclaration with its syntax tree
pub fn parse_class_declaration_from_source<I>(source: &str, tokens: I) -> ClassDeclaration
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let mut sink = EventSink::new();
    class::parse_class_declaration(source, tokens, &mut sink);
    let tree = TreeBuilder::new(source, sink.into_events()).build();
    ClassDeclaration {
        syntax: tree,
        span: 0..source.len(),
    }
}

/// Convenience function to parse a declaration item from source and tokens
/// Returns events - caller must decide how to convert to DeclarationItem
pub fn parse_declaration_item_events<I>(source: &str, tokens: I) -> Vec<event::Event>
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let mut sink = EventSink::new();
    declaration_item::parse_declaration_item(source, tokens, &mut sink);
    sink.into_events()
}

/// Convenience function to parse a source file (multiple declarations) from source and tokens
/// Returns a ParseResult with the syntax tree and any errors
pub fn parse_source_file_from_source<I>(source: &str, tokens: I) -> ParseResult
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    Parser::parse(source, tokens, parse_source_file)
}
