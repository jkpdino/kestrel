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

/// A parse error with a message
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// The error message
    pub message: String,
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
                Event::Error(msg) => Some(ParseError {
                    message: msg.clone(),
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
}
