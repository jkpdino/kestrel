//! Expression parsing
//!
//! This module provides parsing for Kestrel expressions.
//! Currently supports:
//! - Unit expression: ()

use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::skip_trivia;

/// Represents an expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl Expression {
    /// Create a new Expression from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the kind of this expression
    pub fn kind(&self) -> SyntaxKind {
        self.syntax
            .children()
            .next()
            .map(|child| child.kind())
            .unwrap_or(SyntaxKind::Error)
    }

    /// Check if this is a unit expression
    pub fn is_unit(&self) -> bool {
        self.kind() == SyntaxKind::ExprUnit
    }
}

/// Internal enum to distinguish between expression variants during parsing
#[derive(Debug, Clone)]
pub enum ExprVariant {
    /// Unit expression: ()
    Unit(Span, Span), // (lparen, rparen)
}

/// Parser for expressions
///
/// Currently supports:
/// - Unit expression: ()
pub fn expr_parser() -> impl Parser<Token, ExprVariant, Error = Simple<Token>> + Clone {
    // Unit expression: ()
    let unit = skip_trivia()
        .ignore_then(just(Token::LParen).map_with_span(|_, span| span))
        .then(
            skip_trivia()
                .ignore_then(just(Token::RParen).map_with_span(|_, span| span))
        )
        .map(|(lparen, rparen)| ExprVariant::Unit(lparen, rparen));

    unit
}

/// Emit events for any expression variant
pub fn emit_expr_variant(sink: &mut EventSink, variant: &ExprVariant) {
    match variant {
        ExprVariant::Unit(lparen, rparen) => {
            emit_unit_expr(sink, lparen.clone(), rparen.clone());
        }
    }
}

/// Emit events for a unit expression
pub fn emit_unit_expr(sink: &mut EventSink, lparen: Span, rparen: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprUnit);
    sink.add_token(SyntaxKind::LParen, lparen);
    sink.add_token(SyntaxKind::RParen, rparen);
    sink.finish_node(); // Finish ExprUnit
    sink.finish_node(); // Finish Expression
}

/// Parse an expression and emit events
pub fn parse_expr<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match expr_parser().parse(stream) {
        Ok(variant) => {
            emit_expr_variant(sink, &variant);
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

    fn parse_expr_from_source(source: &str) -> Expression {
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let mut sink = EventSink::new();
        parse_expr(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        Expression {
            syntax: tree,
            span: 0..source.len(),
        }
    }

    #[test]
    fn test_unit_expression() {
        let source = "()";
        let expr = parse_expr_from_source(source);

        assert!(expr.is_unit());
    }

    #[test]
    fn test_unit_expression_with_whitespace() {
        let source = "  ()  ";
        let expr = parse_expr_from_source(source);

        assert!(expr.is_unit());
    }
}
