//! Type parameter parsing for generics
//!
//! This module handles parsing of:
//! - Type parameter lists: `[T, U, V]`
//! - Type parameters with defaults: `[T, U = Int]`
//! - Where clauses: `where T: Proto and Proto2, U: Other`
//! - Type argument lists: `[Int, String]` in type use positions

use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::SyntaxKind;

use crate::event::EventSink;
use crate::common::skip_trivia;

/// Raw parsed data for a single type parameter
/// Syntax: T or T = Default
#[derive(Debug, Clone)]
pub struct TypeParameterData {
    /// The name of the type parameter
    pub name: Span,
    /// Optional default type (path segments)
    pub default: Option<Vec<Span>>,
}

/// Raw parsed data for a type argument (in use position)
/// Syntax: SomeType or SomeType[Args]
#[derive(Debug, Clone)]
pub struct TypeArgumentData {
    /// Path segments for the type
    pub path: Vec<Span>,
    /// Optional type arguments for this type
    pub args: Option<Vec<TypeArgumentData>>,
}

/// Raw parsed data for a type bound
/// Syntax: T: Proto and Proto2
#[derive(Debug, Clone)]
pub struct TypeBoundData {
    /// The type parameter being constrained
    pub param: Span,
    /// The protocols/bounds (connected by `and`)
    pub bounds: Vec<Vec<Span>>, // Each bound is a path (e.g., Proto or A.B.Proto)
}

/// Raw parsed data for a where clause
/// Syntax: where T: Proto, U: Other
#[derive(Debug, Clone)]
pub struct WhereClauseData {
    /// The `where` keyword span
    pub where_span: Span,
    /// The type bounds
    pub bounds: Vec<TypeBoundData>,
}

/// Parser for a path (used in type positions): Ident or Ident.Ident.Ident
fn path_parser() -> impl Parser<Token, Vec<Span>, Error = Simple<Token>> + Clone {
    skip_trivia()
        .ignore_then(
            filter_map(|span, token| match token {
                Token::Identifier => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            })
            .separated_by(just(Token::Dot))
            .at_least(1)
        )
}

/// Parser for a single type argument (recursive to handle nested generics)
fn type_argument_parser() -> impl Parser<Token, TypeArgumentData, Error = Simple<Token>> + Clone {
    recursive(|type_arg| {
        // A single type argument: Path optionally followed by [Args]
        path_parser()
            .then(
                skip_trivia()
                    .ignore_then(just(Token::LBracket))
                    .ignore_then(
                        type_arg.clone()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                    )
                    .then_ignore(skip_trivia())
                    .then_ignore(just(Token::RBracket))
                    .or_not()
            )
            .map(|(path, args)| TypeArgumentData { path, args })
    })
}

/// Parser for type arguments: [Type, Type, ...]
/// Handles nested type arguments like Foo[Bar[Baz]]
pub fn type_argument_list_parser() -> impl Parser<Token, Vec<TypeArgumentData>, Error = Simple<Token>> + Clone {
    skip_trivia()
        .ignore_then(just(Token::LBracket))
        .ignore_then(
            type_argument_parser()
                .separated_by(just(Token::Comma))
                .allow_trailing()
        )
        .then_ignore(skip_trivia())
        .then_ignore(just(Token::RBracket))
}

/// Parser for optional type arguments after a path
/// Returns (path, optional args)
pub fn path_with_optional_args_parser() -> impl Parser<Token, TypeArgumentData, Error = Simple<Token>> + Clone {
    path_parser()
        .then(
            type_argument_list_parser().or_not()
        )
        .map(|(path, args)| TypeArgumentData { path, args })
}

/// Parser for a single type parameter: T or T = Default
fn type_parameter_parser() -> impl Parser<Token, TypeParameterData, Error = Simple<Token>> + Clone {
    skip_trivia()
        .ignore_then(
            filter_map(|span, token| match token {
                Token::Identifier => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            })
        )
        .then(
            // Optional default: = Type
            skip_trivia()
                .ignore_then(just(Token::Equals))
                .ignore_then(path_parser())
                .or_not()
        )
        .map(|(name, default)| TypeParameterData { name, default })
}

/// Parser for type parameter list: [T, U, V] or [T, U = String]
pub fn type_parameter_list_parser() -> impl Parser<Token, (Span, Vec<TypeParameterData>, Span), Error = Simple<Token>> + Clone {
    skip_trivia()
        .ignore_then(just(Token::LBracket).map_with_span(|_, span| span))
        .then(
            type_parameter_parser()
                .separated_by(just(Token::Comma))
                .allow_trailing()
        )
        .then(
            skip_trivia()
                .ignore_then(just(Token::RBracket).map_with_span(|_, span| span))
        )
        .map(|((lbracket, params), rbracket)| (lbracket, params, rbracket))
}

/// Parser for a single type bound: T: Proto and Proto2
fn type_bound_parser() -> impl Parser<Token, TypeBoundData, Error = Simple<Token>> + Clone {
    skip_trivia()
        .ignore_then(
            filter_map(|span, token| match token {
                Token::Identifier => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            })
        )
        .then_ignore(skip_trivia())
        .then_ignore(just(Token::Colon))
        .then(
            // Bounds separated by `and`
            path_parser()
                .separated_by(
                    skip_trivia()
                        .ignore_then(just(Token::And))
                        .ignore_then(skip_trivia())
                )
                .at_least(1)
        )
        .map(|(param, bounds)| TypeBoundData { param, bounds })
}

/// Parser for where clause: where T: Proto, U: Other
pub fn where_clause_parser() -> impl Parser<Token, WhereClauseData, Error = Simple<Token>> + Clone {
    skip_trivia()
        .ignore_then(just(Token::Where).map_with_span(|_, span| span))
        .then(
            type_bound_parser()
                .separated_by(just(Token::Comma))
                .at_least(1)
        )
        .map(|(where_span, bounds)| WhereClauseData { where_span, bounds })
}

/// Emit events for a type parameter list
pub fn emit_type_parameter_list(sink: &mut EventSink, lbracket: Span, params: Vec<TypeParameterData>, rbracket: Span) {
    sink.start_node(SyntaxKind::TypeParameterList);
    sink.add_token(SyntaxKind::LBracket, lbracket);

    for param in params {
        emit_type_parameter(sink, param);
    }

    sink.add_token(SyntaxKind::RBracket, rbracket);
    sink.finish_node();
}

/// Emit events for a single type parameter
fn emit_type_parameter(sink: &mut EventSink, param: TypeParameterData) {
    sink.start_node(SyntaxKind::TypeParameter);

    // Emit name
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, param.name);
    sink.finish_node();

    // Emit default if present
    if let Some(default_path) = param.default {
        sink.start_node(SyntaxKind::DefaultType);
        // Wrap in Ty -> TyPath -> Path for consistency with type extractor
        sink.start_node(SyntaxKind::Ty);
        sink.start_node(SyntaxKind::TyPath);
        emit_path(sink, &default_path);
        sink.finish_node(); // TyPath
        sink.finish_node(); // Ty
        sink.finish_node(); // DefaultType
    }

    sink.finish_node();
}

/// Emit events for a where clause
pub fn emit_where_clause(sink: &mut EventSink, data: WhereClauseData) {
    sink.start_node(SyntaxKind::WhereClause);
    sink.add_token(SyntaxKind::Where, data.where_span);

    for bound in data.bounds {
        emit_type_bound(sink, bound);
    }

    sink.finish_node();
}

/// Emit events for a type bound
fn emit_type_bound(sink: &mut EventSink, bound: TypeBoundData) {
    sink.start_node(SyntaxKind::TypeBound);

    // Emit the constrained parameter name
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, bound.param);
    sink.finish_node();

    // Emit each bound path
    for bound_path in bound.bounds {
        emit_path(sink, &bound_path);
    }

    sink.finish_node();
}

/// Emit events for type argument list
pub fn emit_type_argument_list(sink: &mut EventSink, args: &[TypeArgumentData]) {
    sink.start_node(SyntaxKind::TypeArgumentList);

    for arg in args {
        emit_type_argument(sink, arg);
    }

    sink.finish_node();
}

/// Emit events for a single type argument
fn emit_type_argument(sink: &mut EventSink, arg: &TypeArgumentData) {
    sink.start_node(SyntaxKind::Ty);
    sink.start_node(SyntaxKind::TyPath);
    emit_path(sink, &arg.path);

    // Emit nested type arguments if present
    if let Some(ref nested_args) = arg.args {
        emit_type_argument_list(sink, nested_args);
    }

    sink.finish_node(); // TyPath
    sink.finish_node(); // Ty
}

/// Helper to emit a path
fn emit_path(sink: &mut EventSink, segments: &[Span]) {
    sink.start_node(SyntaxKind::Path);

    let mut prev_end: Option<usize> = None;
    for span in segments.iter() {
        if let Some(end) = prev_end {
            // Add dot separator - span is between previous segment's end and current segment's start
            // The dot should be a single character somewhere in this gap
            let dot_start = end;
            let dot_end = (end + 1).min(span.start);
            sink.add_token(SyntaxKind::Dot, dot_start..dot_end);
        }
        sink.start_node(SyntaxKind::PathElement);
        sink.add_token(SyntaxKind::Identifier, span.clone());
        sink.finish_node();
        prev_end = Some(span.end);
    }

    sink.finish_node();
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    fn parse_type_params(source: &str) -> Option<(Span, Vec<TypeParameterData>, Span)> {
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();
        let end_pos = source.len();
        let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens.into_iter());
        type_parameter_list_parser().parse(stream).ok()
    }

    fn parse_where(source: &str) -> Option<WhereClauseData> {
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();
        let end_pos = source.len();
        let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens.into_iter());
        where_clause_parser().parse(stream).ok()
    }

    fn parse_type_args(source: &str) -> Option<Vec<TypeArgumentData>> {
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();
        let end_pos = source.len();
        let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens.into_iter());
        type_argument_list_parser().parse(stream).ok()
    }

    #[test]
    fn test_single_type_parameter() {
        let result = parse_type_params("[T]");
        assert!(result.is_some());
        let (_, params, _) = result.unwrap();
        assert_eq!(params.len(), 1);
        assert!(params[0].default.is_none());
    }

    #[test]
    fn test_multiple_type_parameters() {
        let result = parse_type_params("[T, U, V]");
        assert!(result.is_some());
        let (_, params, _) = result.unwrap();
        assert_eq!(params.len(), 3);
    }

    #[test]
    fn test_type_parameter_with_default() {
        let result = parse_type_params("[T = Int]");
        assert!(result.is_some());
        let (_, params, _) = result.unwrap();
        assert_eq!(params.len(), 1);
        assert!(params[0].default.is_some());
    }

    #[test]
    fn test_mixed_type_parameters() {
        let result = parse_type_params("[K, V = String]");
        assert!(result.is_some());
        let (_, params, _) = result.unwrap();
        assert_eq!(params.len(), 2);
        assert!(params[0].default.is_none());
        assert!(params[1].default.is_some());
    }

    #[test]
    fn test_where_clause_single_bound() {
        let result = parse_where("where T: Equatable");
        assert!(result.is_some());
        let data = result.unwrap();
        assert_eq!(data.bounds.len(), 1);
        assert_eq!(data.bounds[0].bounds.len(), 1);
    }

    #[test]
    fn test_where_clause_and_bounds() {
        let result = parse_where("where T: Equatable and Hashable");
        assert!(result.is_some());
        let data = result.unwrap();
        assert_eq!(data.bounds.len(), 1);
        assert_eq!(data.bounds[0].bounds.len(), 2);
    }

    #[test]
    fn test_where_clause_multiple_params() {
        let result = parse_where("where T: Equatable, U: Serializable");
        assert!(result.is_some());
        let data = result.unwrap();
        assert_eq!(data.bounds.len(), 2);
    }

    #[test]
    fn test_type_arguments_simple() {
        let result = parse_type_args("[Int]");
        assert!(result.is_some());
        let args = result.unwrap();
        assert_eq!(args.len(), 1);
        assert!(args[0].args.is_none());
    }

    #[test]
    fn test_type_arguments_multiple() {
        let result = parse_type_args("[Int, String]");
        assert!(result.is_some());
        let args = result.unwrap();
        assert_eq!(args.len(), 2);
    }

    #[test]
    fn test_type_arguments_nested() {
        let result = parse_type_args("[List[Int]]");
        assert!(result.is_some());
        let args = result.unwrap();
        assert_eq!(args.len(), 1);
        assert!(args[0].args.is_some());
        let nested = args[0].args.as_ref().unwrap();
        assert_eq!(nested.len(), 1);
    }

    #[test]
    fn test_type_arguments_deeply_nested() {
        let result = parse_type_args("[Map[String, List[Int]]]");
        assert!(result.is_some());
        let args = result.unwrap();
        assert_eq!(args.len(), 1);
        let nested = args[0].args.as_ref().unwrap();
        assert_eq!(nested.len(), 2); // String and List[Int]
    }
}
