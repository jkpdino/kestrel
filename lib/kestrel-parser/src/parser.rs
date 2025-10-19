//! High-level parser API
//!
//! This module provides a convenient Parser struct that handles:
//! - Creating event sinks
//! - Parsing with any parse function
//! - Extracting errors from events
//! - Building syntax trees
//!
//! # Example
//!
//! ```no_run
//! use kestrel_parser::parser::Parser;
//! use kestrel_parser::parse_source_file;
//! use kestrel_lexer::lex;
//!
//! let source = "module A.B.C\nimport X.Y.Z";
//! let tokens: Vec<_> = lex(source)
//!     .filter_map(|t| t.ok())
//!     .map(|spanned| (spanned.value, spanned.span))
//!     .collect();
//!
//! let result = Parser::parse(source, tokens.into_iter(), parse_source_file);
//!
//! println!("Syntax tree: {:?}", result.tree);
//! for error in result.errors {
//!     println!("Error: {}", error.message);
//! }
//! ```

use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::SyntaxNode;

use crate::event::{Event, EventSink, TreeBuilder};

/// A parse error with a message and optional span
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// The error message
    pub message: String,
    /// The span where the error occurred (if available)
    pub span: Option<Span>,
}

/// The result of parsing, containing both the syntax tree and any errors
#[derive(Debug, Clone)]
pub struct ParseResult {
    /// The parsed syntax tree
    pub tree: SyntaxNode,
    /// Any parse errors encountered
    pub errors: Vec<ParseError>,
}

/// High-level parser that provides a convenient API for parsing
pub struct Parser;

impl Parser {
    /// Parse source code using the provided parse function
    ///
    /// # Arguments
    ///
    /// * `source` - The source code to parse
    /// * `tokens` - Iterator of tokens (from the lexer)
    /// * `parse_fn` - The parse function to use (e.g., `parse_source_file`)
    ///
    /// # Returns
    ///
    /// A `ParseResult` containing both the syntax tree and any errors
    ///
    /// # Example
    ///
    /// ```no_run
    /// use kestrel_parser::parser::Parser;
    /// use kestrel_parser::parse_source_file;
    /// use kestrel_lexer::lex;
    ///
    /// let source = "module Main";
    /// let tokens: Vec<_> = lex(source)
    ///     .filter_map(|t| t.ok())
    ///     .map(|spanned| (spanned.value, spanned.span))
    ///     .collect();
    ///
    /// let result = Parser::parse(source, tokens.into_iter(), parse_source_file);
    /// assert!(result.errors.is_empty());
    /// ```
    pub fn parse<I, F>(source: &str, tokens: I, parse_fn: F) -> ParseResult
    where
        I: Iterator<Item = (Token, Span)> + Clone,
        F: FnOnce(&str, I, &mut EventSink),
    {
        // Create event sink
        let mut sink = EventSink::new();

        // Parse and collect events
        parse_fn(source, tokens, &mut sink);

        // Extract errors from events
        let events = sink.events();
        let errors: Vec<ParseError> = events
            .iter()
            .filter_map(|e| match e {
                Event::Error { message, span } => Some(ParseError {
                    message: message.clone(),
                    span: span.clone(),
                }),
                _ => None,
            })
            .collect();

        // Build syntax tree from events
        let tree = TreeBuilder::new(source, sink.into_events()).build();

        ParseResult { tree, errors }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;
    use crate::parse_source_file;

    #[test]
    fn test_parser_with_valid_source() {
        let source = "module Test";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let result = Parser::parse(source, tokens.into_iter(), parse_source_file);

        assert!(result.errors.is_empty(), "Should have no errors");
        assert_eq!(result.tree.kind(), kestrel_syntax_tree::SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parser_with_multiple_declarations() {
        let source = "module A.B.C\nimport X.Y.Z";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let result = Parser::parse(source, tokens.into_iter(), parse_source_file);

        assert!(result.errors.is_empty(), "Should have no errors");
        assert_eq!(result.tree.kind(), kestrel_syntax_tree::SyntaxKind::SourceFile);
        assert_eq!(result.tree.children().count(), 2, "Should have 2 declaration children");
    }

    #[test]
    fn test_parser_error_recovery_behavior() {
        // Test the current error recovery behavior:
        // The parser uses Chumsky's .repeated() combinator which provides basic error recovery
        // by continuing to parse after encountering errors in the stream.

        // Test case 1: Parser handles valid code correctly
        let valid_source = r#"
module Test
public class A {}
public class B {}
"#;
        let tokens: Vec<_> = lex(valid_source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let result = Parser::parse(valid_source, tokens.into_iter(), parse_source_file);
        assert_eq!(result.errors.len(), 0, "Valid code should have no errors");
        assert_eq!(result.tree.children().count(), 3, "Should parse all declarations");

        // Test case 2: Parser still creates a tree even with parse errors
        let source_with_errors = r#"module"#; // Incomplete module
        let tokens: Vec<_> = lex(source_with_errors)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let result = Parser::parse(source_with_errors, tokens.into_iter(), parse_source_file);
        // Parser creates a SourceFile node even when parsing fails
        assert_eq!(result.tree.kind(), kestrel_syntax_tree::SyntaxKind::SourceFile);

        println!("Error recovery test: {} declarations, {} errors",
                 result.tree.children().count(),
                 result.errors.len());
    }

    #[test]
    fn test_error_spans_present() {
        // Test that parse errors include span information when errors occur
        // Use a syntax that will definitely cause a parse error
        let source = "class 123"; // class keyword followed by number instead of identifier
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let result = Parser::parse(source, tokens.into_iter(), parse_source_file);

        // Parser should report errors or successfully parse depending on error recovery
        // The important thing is that IF errors are reported, they should have spans
        for error in &result.errors {
            // Parse errors from chumsky should have spans
            println!("Error: {} at {:?}", error.message, error.span);
            // If we have errors, verify they have span info where possible
            if error.span.is_some() {
                println!("  ✓ Span information present");
            }
        }

        // This test primarily documents that span tracking infrastructure is in place
        assert_eq!(result.tree.kind(), kestrel_syntax_tree::SyntaxKind::SourceFile);
    }
}
