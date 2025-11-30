//! Code block parsing
//!
//! This module provides parsing for Kestrel code blocks.
//! A code block has the form: { statement; statement; expression }
//!
//! The trailing expression (without semicolon) determines the block's value and type.
//! If the last item has a semicolon, the block evaluates to unit ().

use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::skip_trivia;
use crate::expr::{expr_parser, ExprVariant, emit_expr_variant};
use crate::stmt::{stmt_parser, StmtVariant, emit_stmt_variant};

/// Represents a code block
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeBlock {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl CodeBlock {
    /// Create a new CodeBlock from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Check if the code block is empty (just braces)
    pub fn is_empty(&self) -> bool {
        // Check if there are no statement or expression children
        self.syntax
            .children()
            .filter(|child| {
                matches!(
                    child.kind(),
                    SyntaxKind::Statement | SyntaxKind::Expression
                )
            })
            .count() == 0
    }

    /// Check if the block has a trailing expression (no semicolon)
    pub fn has_trailing_expression(&self) -> bool {
        // If the last child (excluding closing brace) is an Expression, it's trailing
        self.syntax
            .children()
            .filter(|child| {
                matches!(
                    child.kind(),
                    SyntaxKind::Statement | SyntaxKind::Expression
                )
            })
            .last()
            .map(|child| child.kind() == SyntaxKind::Expression)
            .unwrap_or(false)
    }
}

/// An item in a code block - either a statement or a trailing expression
#[derive(Debug, Clone)]
pub enum BlockItem {
    /// A statement (has semicolon)
    Statement(StmtVariant),
    /// A trailing expression (no semicolon)
    TrailingExpression(ExprVariant),
}

/// Raw parsed data for a code block
#[derive(Debug, Clone)]
pub struct CodeBlockData {
    /// Left brace span
    pub lbrace: Span,
    /// Items in the block (statements and optional trailing expression)
    pub items: Vec<BlockItem>,
    /// Right brace span
    pub rbrace: Span,
}

/// Parser for a code block
///
/// Syntax: { statement* expression? }
///
/// The parser handles:
/// - Empty blocks: { }
/// - Statement-only blocks: { stmt; stmt; }
/// - Trailing expression blocks: { stmt; expr }
pub fn code_block_parser() -> impl Parser<Token, CodeBlockData, Error = Simple<Token>> + Clone {
    skip_trivia()
        .ignore_then(just(Token::LBrace).map_with_span(|_, span| span))
        .then(
            // Parse zero or more items
            // An item is either:
            // 1. A statement (ends with semicolon)
            // 2. A trailing expression (no semicolon, must be last)
            code_block_items_parser()
        )
        .then(
            skip_trivia()
                .ignore_then(just(Token::RBrace).map_with_span(|_, span| span))
        )
        .map(|((lbrace, items), rbrace)| {
            CodeBlockData {
                lbrace,
                items,
                rbrace,
            }
        })
}

/// Parser for the items inside a code block
fn code_block_items_parser() -> impl Parser<Token, Vec<BlockItem>, Error = Simple<Token>> + Clone {
    // We need to handle the distinction between:
    // - Statements (have semicolon)
    // - Trailing expressions (no semicolon, must be last)
    //
    // Strategy: Try to parse statements repeatedly, then optionally parse a trailing expression

    stmt_parser()
        .map(BlockItem::Statement)
        .repeated()
        .then(
            // Optional trailing expression
            expr_parser()
                .map(BlockItem::TrailingExpression)
                .or_not()
        )
        .map(|(mut statements, trailing)| {
            if let Some(expr) = trailing {
                statements.push(expr);
            }
            statements
        })
}

/// Emit events for a code block
pub fn emit_code_block(sink: &mut EventSink, data: &CodeBlockData) {
    sink.start_node(SyntaxKind::CodeBlock);
    sink.add_token(SyntaxKind::LBrace, data.lbrace.clone());

    for item in &data.items {
        match item {
            BlockItem::Statement(stmt) => {
                emit_stmt_variant(sink, stmt);
            }
            BlockItem::TrailingExpression(expr) => {
                emit_expr_variant(sink, expr);
            }
        }
    }

    sink.add_token(SyntaxKind::RBrace, data.rbrace.clone());
    sink.finish_node();
}

/// Parse a code block and emit events
pub fn parse_code_block<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match code_block_parser().parse(stream) {
        Ok(data) => {
            emit_code_block(sink, &data);
        }
        Err(errors) => {
            for error in errors {
                let span = error.span();
                sink.error_at(format!("Parse error: {:?}", error), span);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    fn parse_block_from_source(source: &str) -> CodeBlock {
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let mut sink = EventSink::new();
        parse_code_block(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        CodeBlock {
            syntax: tree,
            span: 0..source.len(),
        }
    }

    #[test]
    fn test_empty_block() {
        let source = "{ }";
        let block = parse_block_from_source(source);

        assert!(block.is_empty());
        assert!(!block.has_trailing_expression());
    }

    #[test]
    fn test_block_with_trailing_expression() {
        let source = "{ () }";
        let block = parse_block_from_source(source);

        assert!(!block.is_empty());
        assert!(block.has_trailing_expression());
    }

    #[test]
    fn test_block_with_statement() {
        let source = "{ (); }";
        let block = parse_block_from_source(source);

        assert!(!block.is_empty());
        assert!(!block.has_trailing_expression());
    }

    #[test]
    fn test_block_with_variable_declaration() {
        let source = "{ let x: Int = (); }";
        let block = parse_block_from_source(source);

        assert!(!block.is_empty());
        assert!(!block.has_trailing_expression());
    }

    #[test]
    fn test_block_with_statements_and_trailing_expr() {
        let source = "{ let x: Int = (); () }";
        let block = parse_block_from_source(source);

        assert!(!block.is_empty());
        assert!(block.has_trailing_expression());
    }
}
