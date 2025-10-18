use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};

/// Represents a class declaration: (visibility)? class Name { ... }
///
/// The declaration is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDeclaration {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl ClassDeclaration {
    /// Create a new ClassDeclaration from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the class name from this declaration
    pub fn name(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tok| tok.kind() == SyntaxKind::Identifier)
            .map(|tok| tok.text().to_string())
    }

    /// Get the visibility modifier if present
    pub fn visibility(&self) -> Option<SyntaxKind> {
        self.syntax
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tok| {
                matches!(
                    tok.kind(),
                    SyntaxKind::Public
                        | SyntaxKind::Private
                        | SyntaxKind::Internal
                        | SyntaxKind::Fileprivate
                )
            })
            .map(|tok| tok.kind())
    }
}

/// Internal Chumsky parser for optional visibility modifier
fn visibility_parser_internal(
) -> impl Parser<Token, Option<(Token, Span)>, Error = Simple<Token>> + Clone {
    filter_map(|span, token| match token {
        Token::Public | Token::Private | Token::Internal | Token::Fileprivate => {
            Ok((token, span))
        }
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .or_not()
}

/// Internal Chumsky parser for class declaration
/// Returns: (visibility, class_span, name_span, lbrace_span, rbrace_span)
fn class_declaration_parser_internal() -> impl Parser<
    Token,
    (Option<(Token, Span)>, Span, Span, Span, Span),
    Error = Simple<Token>,
> + Clone {
    visibility_parser_internal()
        .then(just(Token::Class).map_with_span(|_, span| span))
        .then(filter_map(|span, token| match token {
            Token::Identifier => Ok(span),
            _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
        }))
        .then(just(Token::LBrace).map_with_span(|_, span| span))
        .then(just(Token::RBrace).map_with_span(|_, span| span))
        .map(
            |((((visibility, class_span), name_span), lbrace_span), rbrace_span)| {
                (visibility, class_span, name_span, lbrace_span, rbrace_span)
            },
        )
}

/// Parse a class declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_class_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match class_declaration_parser_internal().parse(stream) {
        Ok((visibility, class_span, name_span, lbrace_span, rbrace_span)) => {
            emit_class_declaration(
                sink,
                visibility,
                class_span,
                name_span,
                lbrace_span,
                rbrace_span,
            );
        }
        Err(errors) => {
            // Emit error events for each parse error
            for error in errors {
                sink.error(format!("Parse error: {:?}", error));
            }
        }
    }
}

/// Emit events for a class declaration
/// Internal helper function
fn emit_class_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    class_span: Span,
    name_span: Span,
    lbrace_span: Span,
    rbrace_span: Span,
) {
    sink.start_node(SyntaxKind::ClassDeclaration);

    // Emit visibility modifier if present
    if let Some((vis_token, vis_span)) = visibility {
        let vis_kind = match vis_token {
            Token::Public => SyntaxKind::Public,
            Token::Private => SyntaxKind::Private,
            Token::Internal => SyntaxKind::Internal,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            _ => unreachable!("visibility_parser_internal only returns visibility tokens"),
        };
        sink.add_token(vis_kind, vis_span);
    }

    sink.add_token(SyntaxKind::Class, class_span);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.add_token(SyntaxKind::LBrace, lbrace_span);
    // TODO: Add support for declaration items in the body
    sink.add_token(SyntaxKind::RBrace, rbrace_span);
    sink.finish_node();
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    #[test]
    fn test_class_declaration_basic() {
        let source = "class Foo { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_class_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ClassDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Foo".to_string()));
        assert_eq!(decl.visibility(), None);
        assert_eq!(decl.syntax.kind(), SyntaxKind::ClassDeclaration);
    }

    #[test]
    fn test_class_declaration_with_visibility() {
        let source = "public class Bar { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_class_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ClassDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Bar".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
        assert_eq!(decl.syntax.kind(), SyntaxKind::ClassDeclaration);
    }

    #[test]
    fn test_class_declaration_private() {
        let source = "private class Baz { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_class_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ClassDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Baz".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Private));
    }

    #[test]
    fn test_class_declaration_internal() {
        let source = "internal class Qux { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_class_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ClassDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Qux".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Internal));
    }

    #[test]
    fn test_class_declaration_fileprivate() {
        let source = "fileprivate class Quux { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_class_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ClassDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Quux".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Fileprivate));
    }
}
