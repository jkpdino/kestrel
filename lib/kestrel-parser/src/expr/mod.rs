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

    /// Check if this is an integer literal
    pub fn is_integer(&self) -> bool {
        self.kind() == SyntaxKind::ExprInteger
    }

    /// Check if this is a float literal
    pub fn is_float(&self) -> bool {
        self.kind() == SyntaxKind::ExprFloat
    }

    /// Check if this is a string literal
    pub fn is_string(&self) -> bool {
        self.kind() == SyntaxKind::ExprString
    }

    /// Check if this is a boolean literal
    pub fn is_bool(&self) -> bool {
        self.kind() == SyntaxKind::ExprBool
    }

    /// Check if this is an array literal
    pub fn is_array(&self) -> bool {
        self.kind() == SyntaxKind::ExprArray
    }

    /// Check if this is a tuple literal
    pub fn is_tuple(&self) -> bool {
        self.kind() == SyntaxKind::ExprTuple
    }

    /// Check if this is a grouping expression
    pub fn is_grouping(&self) -> bool {
        self.kind() == SyntaxKind::ExprGrouping
    }

    /// Check if this is a path expression
    pub fn is_path(&self) -> bool {
        self.kind() == SyntaxKind::ExprPath
    }

    /// Check if this is a unary expression
    pub fn is_unary(&self) -> bool {
        self.kind() == SyntaxKind::ExprUnary
    }

    /// Check if this is a null literal
    pub fn is_null(&self) -> bool {
        self.kind() == SyntaxKind::ExprNull
    }

    /// Check if this is a call expression
    pub fn is_call(&self) -> bool {
        self.kind() == SyntaxKind::ExprCall
    }
}

/// A call argument with optional label
#[derive(Debug, Clone)]
pub struct CallArg {
    /// Optional label (identifier before colon)
    pub label: Option<Span>,
    /// The colon after the label (if labeled)
    pub colon: Option<Span>,
    /// The argument expression
    pub value: ExprVariant,
}

/// Internal enum to distinguish between expression variants during parsing
#[derive(Debug, Clone)]
pub enum ExprVariant {
    /// Unit expression: ()
    Unit(Span, Span), // (lparen, rparen)
    /// Integer literal: 42, 0xFF, 0b1010, 0o17
    Integer(Span),
    /// Float literal: 3.14, 1.0e10
    Float(Span),
    /// String literal: "hello"
    String(Span),
    /// Boolean literal: true, false
    Bool(Span),
    /// Null literal: null
    Null(Span),
    /// Array literal: [1, 2, 3]
    Array(Span, Vec<ExprVariant>, Vec<Span>, Span), // (lbracket, elements, commas, rbracket)
    /// Tuple literal: (1, 2, 3)
    Tuple(Span, Vec<ExprVariant>, Vec<Span>, Span), // (lparen, elements, commas, rparen)
    /// Grouping expression: (expr)
    Grouping(Span, Box<ExprVariant>, Span), // (lparen, inner, rparen)
    /// Path expression: a.b.c
    Path(Vec<Span>, Vec<Span>), // (segments, dots)
    /// Unary expression: -expr, !expr
    Unary(Token, Span, Box<ExprVariant>), // (operator_token, operator_span, operand)
    /// Call expression: callee(args)
    Call {
        callee: Box<ExprVariant>,
        lparen: Span,
        arguments: Vec<CallArg>,
        commas: Vec<Span>,
        rparen: Span,
    },
}

/// Parser for expressions
///
/// Supports:
/// - Unit expression: ()
/// - Integer literals: 42, 0xFF, 0b1010, 0o17
/// - Float literals: 3.14, 1.0e10
/// - String literals: "hello"
/// - Boolean literals: true, false
/// - Array literals: [1, 2, 3]
/// - Tuple literals: (1, 2, 3)
/// - Grouping: (expr)
pub fn expr_parser() -> impl Parser<Token, ExprVariant, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        // Integer literal
        let integer = skip_trivia()
            .ignore_then(filter_map(|span, token| match token {
                Token::Integer => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            }))
            .map(ExprVariant::Integer);

        // Float literal
        let float = skip_trivia()
            .ignore_then(filter_map(|span, token| match token {
                Token::Float => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            }))
            .map(ExprVariant::Float);

        // String literal
        let string = skip_trivia()
            .ignore_then(filter_map(|span, token| match token {
                Token::String => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            }))
            .map(ExprVariant::String);

        // Boolean literal
        let boolean = skip_trivia()
            .ignore_then(filter_map(|span, token| match token {
                Token::Boolean => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            }))
            .map(ExprVariant::Bool);

        // Null literal
        let null = skip_trivia()
            .ignore_then(filter_map(|span, token| match token {
                Token::Null => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            }))
            .map(ExprVariant::Null);

        // Path expression: a.b.c
        let path = skip_trivia()
            .ignore_then(filter_map(|span, token| match token {
                Token::Identifier => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            }))
            .then(
                skip_trivia()
                    .ignore_then(just(Token::Dot).map_with_span(|_, span| span))
                    .then(skip_trivia().ignore_then(filter_map(|span, token| match token {
                        Token::Identifier => Ok(span),
                        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
                    })))
                    .repeated()
            )
            .map(|(first, rest)| {
                let mut segments = vec![first];
                let mut dots = Vec::new();
                for (dot, segment) in rest {
                    dots.push(dot);
                    segments.push(segment);
                }
                ExprVariant::Path(segments, dots)
            });

        // Array literal: [elem, elem, ...]
        let array = skip_trivia()
            .ignore_then(just(Token::LBracket).map_with_span(|_, span| span))
            .then(
                expr.clone()
                    .then(
                        skip_trivia()
                            .ignore_then(just(Token::Comma).map_with_span(|_, span| span))
                            .then(skip_trivia().ignore_then(expr.clone()))
                            .repeated()
                    )
                    .then(
                        skip_trivia()
                            .ignore_then(just(Token::Comma).map_with_span(|_, span| span))
                            .or_not()
                    )
                    .map(|((first, rest), trailing)| {
                        let mut elements = vec![first];
                        let mut commas = Vec::new();
                        for (comma, elem) in rest {
                            commas.push(comma);
                            elements.push(elem);
                        }
                        if let Some(tc) = trailing {
                            commas.push(tc);
                        }
                        (elements, commas)
                    })
                    .or_not()
            )
            .then(skip_trivia().ignore_then(just(Token::RBracket).map_with_span(|_, span| span)))
            .map(|((lbracket, contents), rbracket)| {
                let (elements, commas) = contents.unwrap_or_else(|| (vec![], vec![]));
                ExprVariant::Array(lbracket, elements, commas, rbracket)
            });

        // Parenthesized expressions: (), (expr), (expr,), (expr, expr, ...)
        let paren_expr = skip_trivia()
            .ignore_then(just(Token::LParen).map_with_span(|_, span| span))
            .then(
                // Empty parens: ()
                skip_trivia()
                    .ignore_then(just(Token::RParen).map_with_span(|_, span| span))
                    .map(|rparen| ParenContent::Unit(rparen))
                .or(
                    // Non-empty: expr followed by optional comma and more
                    expr.clone()
                        .then(
                            skip_trivia()
                                .ignore_then(just(Token::Comma).map_with_span(|_, span| span))
                                .then(
                                    skip_trivia()
                                        .ignore_then(expr.clone())
                                        .then(
                                            skip_trivia()
                                                .ignore_then(just(Token::Comma).map_with_span(|_, span| span))
                                                .then(skip_trivia().ignore_then(expr.clone()))
                                                .repeated()
                                        )
                                        .then(
                                            skip_trivia()
                                                .ignore_then(just(Token::Comma).map_with_span(|_, span| span))
                                                .or_not()
                                        )
                                        .map(|((second, rest), trailing)| {
                                            let mut elements = vec![second];
                                            let mut commas = Vec::new();
                                            for (comma, elem) in rest {
                                                commas.push(comma);
                                                elements.push(elem);
                                            }
                                            if let Some(tc) = trailing {
                                                commas.push(tc);
                                            }
                                            (elements, commas)
                                        })
                                        .or_not()
                                )
                                .or_not()
                        )
                        .then(skip_trivia().ignore_then(just(Token::RParen).map_with_span(|_, span| span)))
                        .map(|((first, comma_rest), rparen)| {
                            match comma_rest {
                                None => {
                                    // (expr) - grouping
                                    ParenContent::Grouping(first, rparen)
                                }
                                Some((first_comma, None)) => {
                                    // (expr,) - single-element tuple
                                    ParenContent::Tuple(vec![first], vec![first_comma], rparen)
                                }
                                Some((first_comma, Some((more_elems, more_commas)))) => {
                                    // (expr, expr, ...) - multi-element tuple
                                    let mut elements = vec![first];
                                    elements.extend(more_elems);
                                    let mut commas = vec![first_comma];
                                    commas.extend(more_commas);
                                    ParenContent::Tuple(elements, commas, rparen)
                                }
                            }
                        })
                )
            )
            .map(|(lparen, content)| match content {
                ParenContent::Unit(rparen) => ExprVariant::Unit(lparen, rparen),
                ParenContent::Grouping(inner, rparen) => ExprVariant::Grouping(lparen, Box::new(inner), rparen),
                ParenContent::Tuple(elements, commas, rparen) => ExprVariant::Tuple(lparen, elements, commas, rparen),
            });

        // Unary operators: -expr, !expr
        // This must be defined after the other expression parsers so we can use them
        let primary = float
            .or(integer)
            .or(string)
            .or(boolean)
            .or(null)
            .or(array)
            .or(paren_expr)
            .or(path);

        // Argument parser for call expressions: unlabeled or labeled
        // labeled: identifier: expr
        // unlabeled: expr
        let argument = skip_trivia()
            .ignore_then(
                // Try labeled argument: identifier: expr
                filter_map(|span, token| match token {
                    Token::Identifier => Ok(span),
                    _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
                })
                .then(skip_trivia().ignore_then(just(Token::Colon).map_with_span(|_, span| span)))
                .then(skip_trivia().ignore_then(expr.clone()))
                .map(|((label, colon), value)| CallArg {
                    label: Some(label),
                    colon: Some(colon),
                    value,
                })
                // Or unlabeled: just expr
                .or(
                    expr.clone().map(|value| CallArg {
                        label: None,
                        colon: None,
                        value,
                    })
                )
            );

        // Argument list: (arg, arg, ...)
        let arg_list = skip_trivia()
            .ignore_then(just(Token::LParen).map_with_span(|_, span| span))
            .then(
                // Empty arg list
                skip_trivia()
                    .ignore_then(just(Token::RParen).map_with_span(|_, span| span))
                    .map(|rparen| (vec![], vec![], rparen))
                .or(
                    // Non-empty: arg followed by optional commas and more
                    argument.clone()
                        .then(
                            skip_trivia()
                                .ignore_then(just(Token::Comma).map_with_span(|_, span| span))
                                .then(skip_trivia().ignore_then(argument.clone()))
                                .repeated()
                        )
                        .then(
                            skip_trivia()
                                .ignore_then(just(Token::Comma).map_with_span(|_, span| span))
                                .or_not()
                        )
                        .then(skip_trivia().ignore_then(just(Token::RParen).map_with_span(|_, span| span)))
                        .map(|(((first, rest), trailing), rparen)| {
                            let mut arguments = vec![first];
                            let mut commas = Vec::new();
                            for (comma, arg) in rest {
                                commas.push(comma);
                                arguments.push(arg);
                            }
                            if let Some(tc) = trailing {
                                commas.push(tc);
                            }
                            (arguments, commas, rparen)
                        })
                )
            )
            .map(|(lparen, (arguments, commas, rparen))| (lparen, arguments, commas, rparen));

        // Postfix call: expr(args) - can be chained: foo()()
        let postfix = primary.clone()
            .then(arg_list.repeated())
            .map(|(base, calls)| {
                calls.into_iter().fold(base, |callee, (lparen, arguments, commas, rparen)| {
                    ExprVariant::Call {
                        callee: Box::new(callee),
                        lparen,
                        arguments,
                        commas,
                        rparen,
                    }
                })
            });

        let unary = skip_trivia()
            .ignore_then(
                just(Token::Minus).map_with_span(|tok, span| (tok, span))
                    .or(just(Token::Bang).map_with_span(|tok, span| (tok, span)))
            )
            .then(expr.clone())
            .map(|((tok, span), operand)| ExprVariant::Unary(tok, span, Box::new(operand)));

        // Combine all expression parsers
        // Order matters: try unary first to handle -42, then postfix expressions
        unary.or(postfix)
    })
}

/// Helper enum for parsing parenthesized expressions
#[derive(Debug, Clone)]
enum ParenContent {
    Unit(Span),
    Grouping(ExprVariant, Span),
    Tuple(Vec<ExprVariant>, Vec<Span>, Span),
}

/// Emit events for any expression variant
pub fn emit_expr_variant(sink: &mut EventSink, variant: &ExprVariant) {
    match variant {
        ExprVariant::Unit(lparen, rparen) => {
            emit_unit_expr(sink, lparen.clone(), rparen.clone());
        }
        ExprVariant::Integer(span) => {
            emit_integer_expr(sink, span.clone());
        }
        ExprVariant::Float(span) => {
            emit_float_expr(sink, span.clone());
        }
        ExprVariant::String(span) => {
            emit_string_expr(sink, span.clone());
        }
        ExprVariant::Bool(span) => {
            emit_bool_expr(sink, span.clone());
        }
        ExprVariant::Null(span) => {
            emit_null_expr(sink, span.clone());
        }
        ExprVariant::Array(lbracket, elements, commas, rbracket) => {
            emit_array_expr(sink, lbracket.clone(), elements, commas, rbracket.clone());
        }
        ExprVariant::Tuple(lparen, elements, commas, rparen) => {
            emit_tuple_expr(sink, lparen.clone(), elements, commas, rparen.clone());
        }
        ExprVariant::Grouping(lparen, inner, rparen) => {
            emit_grouping_expr(sink, lparen.clone(), inner, rparen.clone());
        }
        ExprVariant::Path(segments, dots) => {
            emit_path_expr(sink, segments, dots);
        }
        ExprVariant::Unary(tok, span, operand) => {
            emit_unary_expr(sink, tok.clone(), span.clone(), operand);
        }
        ExprVariant::Call { callee, lparen, arguments, commas, rparen } => {
            emit_call_expr(sink, callee, lparen.clone(), arguments, commas, rparen.clone());
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

/// Emit events for an integer literal expression
fn emit_integer_expr(sink: &mut EventSink, span: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprInteger);
    sink.add_token(SyntaxKind::Integer, span);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a float literal expression
fn emit_float_expr(sink: &mut EventSink, span: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprFloat);
    sink.add_token(SyntaxKind::Float, span);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a string literal expression
fn emit_string_expr(sink: &mut EventSink, span: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprString);
    sink.add_token(SyntaxKind::String, span);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a boolean literal expression
fn emit_bool_expr(sink: &mut EventSink, span: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprBool);
    sink.add_token(SyntaxKind::Boolean, span);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a null literal expression
fn emit_null_expr(sink: &mut EventSink, span: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprNull);
    sink.add_token(SyntaxKind::Null, span);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for an array literal expression
fn emit_array_expr(sink: &mut EventSink, lbracket: Span, elements: &[ExprVariant], commas: &[Span], rbracket: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprArray);
    sink.add_token(SyntaxKind::LBracket, lbracket);
    for (i, element) in elements.iter().enumerate() {
        emit_expr_variant(sink, element);
        // Add comma after element if there is one
        if i < commas.len() {
            sink.add_token(SyntaxKind::Comma, commas[i].clone());
        }
    }
    sink.add_token(SyntaxKind::RBracket, rbracket);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a tuple literal expression
fn emit_tuple_expr(sink: &mut EventSink, lparen: Span, elements: &[ExprVariant], commas: &[Span], rparen: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprTuple);
    sink.add_token(SyntaxKind::LParen, lparen);
    for (i, element) in elements.iter().enumerate() {
        emit_expr_variant(sink, element);
        // Add comma after element if there is one
        if i < commas.len() {
            sink.add_token(SyntaxKind::Comma, commas[i].clone());
        }
    }
    sink.add_token(SyntaxKind::RParen, rparen);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a grouping expression
fn emit_grouping_expr(sink: &mut EventSink, lparen: Span, inner: &ExprVariant, rparen: Span) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprGrouping);
    sink.add_token(SyntaxKind::LParen, lparen);
    emit_expr_variant(sink, inner);
    sink.add_token(SyntaxKind::RParen, rparen);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a path expression
fn emit_path_expr(sink: &mut EventSink, segments: &[Span], dots: &[Span]) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprPath);
    for (i, segment) in segments.iter().enumerate() {
        sink.add_token(SyntaxKind::Identifier, segment.clone());
        // Add dot after segment if there is one
        if i < dots.len() {
            sink.add_token(SyntaxKind::Dot, dots[i].clone());
        }
    }
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a unary expression
fn emit_unary_expr(sink: &mut EventSink, tok: Token, span: Span, operand: &ExprVariant) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprUnary);
    sink.add_token(SyntaxKind::from(tok), span);
    emit_expr_variant(sink, operand);
    sink.finish_node();
    sink.finish_node();
}

/// Emit events for a call expression
fn emit_call_expr(
    sink: &mut EventSink,
    callee: &ExprVariant,
    lparen: Span,
    arguments: &[CallArg],
    commas: &[Span],
    rparen: Span,
) {
    sink.start_node(SyntaxKind::Expression);
    sink.start_node(SyntaxKind::ExprCall);

    // Emit the callee expression
    emit_expr_variant(sink, callee);

    // Emit the argument list
    sink.start_node(SyntaxKind::ArgumentList);
    sink.add_token(SyntaxKind::LParen, lparen);

    for (i, arg) in arguments.iter().enumerate() {
        sink.start_node(SyntaxKind::Argument);

        // If labeled, emit label and colon
        if let (Some(label), Some(colon)) = (&arg.label, &arg.colon) {
            sink.add_token(SyntaxKind::Identifier, label.clone());
            sink.add_token(SyntaxKind::Colon, colon.clone());
        }

        // Emit the argument value
        emit_expr_variant(sink, &arg.value);

        sink.finish_node(); // Argument

        // Add comma after argument if there is one
        if i < commas.len() {
            sink.add_token(SyntaxKind::Comma, commas[i].clone());
        }
    }

    sink.add_token(SyntaxKind::RParen, rparen);
    sink.finish_node(); // ArgumentList

    sink.finish_node(); // ExprCall
    sink.finish_node(); // Expression
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

    // ===== Unit Expression Tests =====

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

    // ===== Integer Literal Tests =====

    #[test]
    fn test_integer_decimal() {
        let source = "42";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_integer());
    }

    #[test]
    fn test_integer_hex() {
        let source = "0xFF";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_integer());
    }

    #[test]
    fn test_integer_hex_uppercase() {
        let source = "0XAB";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_integer());
    }

    #[test]
    fn test_integer_binary() {
        let source = "0b1010";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_integer());
    }

    #[test]
    fn test_integer_octal() {
        let source = "0o755";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_integer());
    }

    // ===== Float Literal Tests =====

    #[test]
    fn test_float_simple() {
        let source = "3.14";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_float());
    }

    #[test]
    fn test_float_scientific() {
        let source = "1.0e10";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_float());
    }

    #[test]
    fn test_float_scientific_negative() {
        let source = "2.5E-3";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_float());
    }

    // ===== String Literal Tests =====

    #[test]
    fn test_string_simple() {
        let source = r#""hello""#;
        let expr = parse_expr_from_source(source);
        assert!(expr.is_string());
    }

    #[test]
    fn test_string_with_escapes() {
        let source = r#""hello\nworld""#;
        let expr = parse_expr_from_source(source);
        assert!(expr.is_string());
    }

    #[test]
    fn test_string_empty() {
        let source = r#""""#;
        let expr = parse_expr_from_source(source);
        assert!(expr.is_string());
    }

    // ===== Boolean Literal Tests =====

    #[test]
    fn test_bool_true() {
        let source = "true";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_bool());
    }

    #[test]
    fn test_bool_false() {
        let source = "false";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_bool());
    }

    // ===== Array Literal Tests =====

    #[test]
    fn test_array_empty() {
        let source = "[]";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    #[test]
    fn test_array_single() {
        let source = "[1]";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    #[test]
    fn test_array_multiple() {
        let source = "[1, 2, 3]";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    #[test]
    fn test_array_trailing_comma() {
        let source = "[1, 2, 3,]";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    #[test]
    fn test_array_nested() {
        let source = "[[1, 2], [3, 4]]";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    #[test]
    fn test_array_mixed_types() {
        let source = r#"[1, "hello", true]"#;
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    // ===== Tuple Literal Tests =====

    #[test]
    fn test_tuple_single_element() {
        // Single element with trailing comma is a tuple
        let source = "(1,)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_tuple());
    }

    #[test]
    fn test_tuple_two_elements() {
        let source = "(1, 2)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_tuple());
    }

    #[test]
    fn test_tuple_multiple() {
        let source = "(1, 2, 3)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_tuple());
    }

    #[test]
    fn test_tuple_trailing_comma() {
        let source = "(1, 2, 3,)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_tuple());
    }

    #[test]
    fn test_tuple_nested() {
        let source = "((1, 2), (3, 4))";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_tuple());
    }

    // ===== Grouping Expression Tests =====

    #[test]
    fn test_grouping_integer() {
        // Single element without trailing comma is grouping
        let source = "(42)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_grouping());
    }

    #[test]
    fn test_grouping_nested() {
        let source = "((42))";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_grouping());
    }

    #[test]
    fn test_grouping_string() {
        let source = r#"("hello")"#;
        let expr = parse_expr_from_source(source);
        assert!(expr.is_grouping());
    }

    // ===== Mixed/Complex Tests =====

    #[test]
    fn test_array_of_tuples() {
        let source = "[(1, 2), (3, 4)]";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    #[test]
    fn test_tuple_of_arrays() {
        let source = "([1, 2], [3, 4])";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_tuple());
    }

    #[test]
    fn test_deeply_nested() {
        let source = "[[(1,)]]";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    // ===== Path Expression Tests =====

    #[test]
    fn test_path_single_segment() {
        let source = "foo";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_path());
    }

    #[test]
    fn test_path_two_segments() {
        let source = "foo.bar";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_path());
    }

    #[test]
    fn test_path_multiple_segments() {
        let source = "a.b.c.d";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_path());
    }

    #[test]
    fn test_path_with_whitespace() {
        let source = "  foo . bar  ";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_path());
    }

    // ===== Unary Expression Tests =====

    #[test]
    fn test_unary_minus_integer() {
        let source = "-42";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_unary());
    }

    #[test]
    fn test_unary_minus_float() {
        let source = "-3.14";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_unary());
    }

    #[test]
    fn test_unary_bang() {
        let source = "!true";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_unary());
    }

    #[test]
    fn test_unary_double_minus() {
        let source = "--42";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_unary());
    }

    #[test]
    fn test_unary_double_bang() {
        let source = "!!false";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_unary());
    }

    #[test]
    fn test_unary_minus_path() {
        let source = "-foo";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_unary());
    }

    #[test]
    fn test_unary_minus_grouped() {
        let source = "-(1)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_unary());
    }

    // ===== Null Literal Tests =====

    #[test]
    fn test_null() {
        let source = "null";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_null());
    }

    #[test]
    fn test_null_in_array() {
        let source = "[null, null]";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_array());
    }

    #[test]
    fn test_null_in_tuple() {
        let source = "(null, 42)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_tuple());
    }

    // ===== Call Expression Tests =====

    #[test]
    fn test_call_no_args() {
        let source = "foo()";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_call_single_arg() {
        let source = "foo(42)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_call_multiple_args() {
        let source = "foo(1, 2, 3)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_call_with_trailing_comma() {
        let source = "foo(1, 2,)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_call_labeled_arg() {
        let source = "foo(x: 42)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_call_mixed_labeled_unlabeled() {
        let source = "foo(1, name: \"test\", 3)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_call_chained() {
        let source = "foo()()";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_method_call() {
        let source = "obj.method()";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_method_call_with_args() {
        let source = "obj.method(1, 2)";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_chained_method_calls() {
        let source = "a.b().c().d()";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }

    #[test]
    fn test_call_with_expression_args() {
        let source = "foo((1, 2), [3, 4])";
        let expr = parse_expr_from_source(source);
        assert!(expr.is_call());
    }
}
