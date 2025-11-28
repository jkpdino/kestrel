//! Common parser combinators shared across multiple parsers
//!
//! This module provides reusable Chumsky parser combinators that are used
//! by multiple parser modules to avoid code duplication.

use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;

/// Check if a token is trivia (whitespace or comment)
pub fn is_trivia(token: &Token) -> bool {
    matches!(token, Token::Whitespace | Token::LineComment | Token::BlockComment)
}

/// Parser that skips trivia tokens
pub fn skip_trivia() -> impl Parser<Token, (), Error = Simple<Token>> + Clone {
    filter(|token: &Token| is_trivia(token))
        .repeated()
        .ignored()
}

/// Wrap a parser to skip leading trivia
pub fn trivia<P, O>(parser: P) -> impl Parser<Token, O, Error = Simple<Token>> + Clone
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone,
{
    skip_trivia().ignore_then(parser)
}

/// Match a specific token, skipping leading trivia
pub fn token(t: Token) -> impl Parser<Token, Span, Error = Simple<Token>> + Clone {
    trivia(just(t).map_with_span(|_, span| span))
}

/// Parse an identifier, skipping leading trivia
pub fn identifier() -> impl Parser<Token, Span, Error = Simple<Token>> + Clone {
    trivia(filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    }))
}

/// Internal Chumsky parser for module path segments
///
/// Parses identifier sequences separated by dots: A.B.C
/// Returns a vector of spans for each identifier segment.
///
/// # Examples
/// - `A` → `[span(A)]`
/// - `A.B.C` → `[span(A), span(B), span(C)]`
pub fn module_path_parser_internal() -> impl Parser<Token, Vec<Span>, Error = Simple<Token>> + Clone {
    identifier()
        .separated_by(token(Token::Dot))
        .at_least(1)
}

/// Internal Chumsky parser for optional visibility modifier
///
/// Parses an optional visibility keyword: public, private, internal, or fileprivate
/// Returns `Some((token, span))` if a visibility modifier is present, `None` otherwise.
///
/// # Examples
/// - `public class Foo` → `Some((Token::Public, span))`
/// - `class Foo` → `None`
pub fn visibility_parser_internal(
) -> impl Parser<Token, Option<(Token, Span)>, Error = Simple<Token>> + Clone {
    trivia(filter_map(|span, token| match token {
        Token::Public | Token::Private | Token::Internal | Token::Fileprivate => {
            Ok((token, span))
        }
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    }))
    .or_not()
}

/// Internal Chumsky parser for module declaration
///
/// Parses: `module A.B.C`
/// Returns: `(module_keyword_span, path_segments)`
///
/// # Examples
/// - `module A` → `(span(module), [span(A)])`
/// - `module A.B.C` → `(span(module), [span(A), span(B), span(C)])`
pub fn module_declaration_parser_internal() -> impl Parser<Token, (Span, Vec<Span>), Error = Simple<Token>> + Clone {
    token(Token::Module)
        .then(module_path_parser_internal())
}

/// Internal parser for import item (identifier or identifier as alias)
///
/// Parses a single import item, optionally with an alias:
/// - `D` → `(span(D), None)`
/// - `D as E` → `(span(D), Some(span(E)))`
///
/// # Examples
/// - `Foo` → `(span(Foo), None)`
/// - `Foo as Bar` → `(span(Foo), Some(span(Bar)))`
pub fn import_item_parser_internal() -> impl Parser<Token, (Span, Option<Span>), Error = Simple<Token>> + Clone {
    identifier()
        .then(
            token(Token::As)
                .ignore_then(identifier())
                .or_not()
        )
}

/// Internal parser for import items list
///
/// Parses a parenthesized list of import items: `(D, E)` or `(D as E, F as G)`
/// Returns a vector of tuples: `(name_span, optional_alias_span)`
///
/// # Examples
/// - `(D, E)` → `[(span(D), None), (span(E), None)]`
/// - `(D as E, F)` → `[(span(D), Some(span(E))), (span(F), None)]`
pub fn import_items_parser_internal() -> impl Parser<Token, Vec<(Span, Option<Span>)>, Error = Simple<Token>> + Clone {
    token(Token::LParen)
        .ignore_then(
            import_item_parser_internal()
                .separated_by(token(Token::Comma))
                .at_least(1)
        )
        .then_ignore(token(Token::RParen))
}

/// Internal parser for import declaration
///
/// Parses all forms of import declarations:
/// - `import A.B.C` (import all)
/// - `import A.B.C as D` (import with alias)
/// - `import A.B.C.(D, E)` (import specific items)
/// - `import A.B.C.(D as E, F as G)` (import items with aliases)
///
/// Returns: `(import_keyword_span, path_segments, optional_alias, optional_items_list)`
///
/// # Examples
/// - `import A.B.C` → `(span(import), [span(A), span(B), span(C)], None, None)`
/// - `import A.B.C as D` → `(span(import), [span(A), span(B), span(C)], Some(span(D)), None)`
/// - `import A.B.C.(D, E)` → `(span(import), [span(A), span(B), span(C)], None, Some([...]))`
pub fn import_declaration_parser_internal() -> impl Parser<Token, (Span, Vec<Span>, Option<Span>, Option<Vec<(Span, Option<Span>)>>), Error = Simple<Token>> + Clone {
    token(Token::Import)
        .then(module_path_parser_internal())
        .then(
            // Optional: either "as Alias" or ".(items)"
            token(Token::As)
                .ignore_then(identifier())
                .map(|alias| (Some(alias), None))
                .or(
                    token(Token::Dot)
                        .ignore_then(import_items_parser_internal())
                        .map(|items| (None, Some(items)))
                )
                .or_not()
        )
        .map(|((import_span, path_segments), alias_or_items)| {
            let (alias, items) = match alias_or_items {
                Some((alias, items)) => (alias, items),
                None => (None, None),
            };
            (import_span, path_segments, alias, items)
        })
}
