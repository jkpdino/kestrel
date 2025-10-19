use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};

/// Represents a function declaration: (visibility)? fn Name(params) (-> ReturnType)? { ... }
///
/// The declaration is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDeclaration {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl FnDeclaration {
    /// Create a new FnDeclaration from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the function name from this declaration
    pub fn name(&self) -> Option<String> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::Name)?
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tok| tok.kind() == SyntaxKind::Identifier)
            .map(|tok| tok.text().to_string())
    }

    /// Get the visibility modifier if present
    pub fn visibility(&self) -> Option<SyntaxKind> {
        let visibility_node = self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::Visibility)?;

        visibility_node
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

    /// Get the return type if present
    pub fn return_type(&self) -> Option<String> {
        let return_type_node = self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::ReturnType)?;

        return_type_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tok| tok.kind() == SyntaxKind::Identifier)
            .map(|tok| tok.text().to_string())
    }

    /// Get the parameters from this function
    pub fn parameters(&self) -> Vec<(String, String)> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::ParameterList)
            .map(|param_list| {
                param_list
                    .children()
                    .filter(|child| child.kind() == SyntaxKind::Parameter)
                    .filter_map(|param| {
                        let tokens: Vec<_> = param
                            .children_with_tokens()
                            .filter_map(|elem| elem.into_token())
                            .filter(|tok| tok.kind() == SyntaxKind::Identifier)
                            .collect();

                        if tokens.len() == 2 {
                            Some((
                                tokens[0].text().to_string(),
                                tokens[1].text().to_string(),
                            ))
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .unwrap_or_default()
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

/// Internal parser for a single parameter: name: Type
fn parameter_parser_internal() -> impl Parser<Token, (Span, Span, Span), Error = Simple<Token>> + Clone {
    filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .then(just(Token::Colon).map_with_span(|_, span| span))
    .then(filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    }))
    .map(|((name_span, colon_span), type_span)| (name_span, colon_span, type_span))
}

/// Internal parser for parameter list: (param1, param2, ...)
fn parameter_list_parser_internal() -> impl Parser<
    Token,
    (Span, Vec<(Span, Span, Span)>, Span),
    Error = Simple<Token>,
> + Clone {
    just(Token::LParen)
        .map_with_span(|_, span| span)
        .then(
            parameter_parser_internal()
                .separated_by(just(Token::Comma))
                .allow_trailing()
        )
        .then(just(Token::RParen).map_with_span(|_, span| span))
        .map(|((lparen_span, params), rparen_span)| (lparen_span, params, rparen_span))
}

/// Internal parser for return type: -> Type
fn return_type_parser_internal() -> impl Parser<Token, (Span, Span), Error = Simple<Token>> + Clone {
    just(Token::Arrow)
        .map_with_span(|_, span| span)
        .then(filter_map(|span, token| match token {
            Token::Identifier => Ok(span),
            _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
        }))
}

/// Internal Chumsky parser for function declaration
fn fn_declaration_parser_internal() -> impl Parser<
    Token,
    (
        Option<(Token, Span)>,
        Span,
        Span,
        Span,
        Vec<(Span, Span, Span)>,
        Span,
        Option<(Span, Span)>,
        Span,
        Span,
    ),
    Error = Simple<Token>,
> + Clone {
    visibility_parser_internal()
        .then(just(Token::Fn).map_with_span(|_, span| span))
        .then(filter_map(|span, token| match token {
            Token::Identifier => Ok(span),
            _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
        }))
        .then(parameter_list_parser_internal())
        .then(return_type_parser_internal().or_not())
        .then(just(Token::LBrace).map_with_span(|_, span| span))
        .then(just(Token::RBrace).map_with_span(|_, span| span))
        .map(
            |((((((visibility, fn_span), name_span), (lparen_span, params, rparen_span)), return_type), lbrace_span), rbrace_span)| {
                (
                    visibility,
                    fn_span,
                    name_span,
                    lparen_span,
                    params,
                    rparen_span,
                    return_type,
                    lbrace_span,
                    rbrace_span,
                )
            },
        )
}

/// Parse a function declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_fn_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match fn_declaration_parser_internal().parse(stream) {
        Ok((
            visibility,
            fn_span,
            name_span,
            lparen_span,
            params,
            rparen_span,
            return_type,
            lbrace_span,
            rbrace_span,
        )) => {
            emit_fn_declaration(
                sink,
                visibility,
                fn_span,
                name_span,
                lparen_span,
                params,
                rparen_span,
                return_type,
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

/// Emit events for a function declaration
/// Internal helper function
#[allow(clippy::too_many_arguments)]
pub(crate) fn emit_fn_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    fn_span: Span,
    name_span: Span,
    lparen_span: Span,
    params: Vec<(Span, Span, Span)>,
    rparen_span: Span,
    return_type: Option<(Span, Span)>,
    lbrace_span: Span,
    rbrace_span: Span,
) {
    sink.start_node(SyntaxKind::FnDeclaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
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
    sink.finish_node(); // Finish Visibility

    sink.add_token(SyntaxKind::Fn, fn_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    // Emit ParameterList node
    sink.start_node(SyntaxKind::ParameterList);
    sink.add_token(SyntaxKind::LParen, lparen_span);

    // Emit each parameter
    for (name_span, colon_span, type_span) in params.iter() {
        sink.start_node(SyntaxKind::Parameter);
        sink.add_token(SyntaxKind::Identifier, name_span.clone());
        sink.add_token(SyntaxKind::Colon, colon_span.clone());
        sink.add_token(SyntaxKind::Identifier, type_span.clone());
        sink.finish_node(); // Finish Parameter

        // Add comma if not the last parameter
        // Note: We don't track comma spans in our simplified parser
        // A full implementation would track them
    }

    sink.add_token(SyntaxKind::RParen, rparen_span);
    sink.finish_node(); // Finish ParameterList

    // Emit ReturnType node (may be empty)
    sink.start_node(SyntaxKind::ReturnType);
    if let Some((arrow_span, type_span)) = return_type {
        sink.add_token(SyntaxKind::Arrow, arrow_span);
        sink.add_token(SyntaxKind::Identifier, type_span);
    }
    sink.finish_node(); // Finish ReturnType

    // Emit FnBody node wrapping the body content
    sink.start_node(SyntaxKind::FnBody);
    sink.add_token(SyntaxKind::LBrace, lbrace_span);
    // Body content would go here (statements, expressions, etc.)
    // For now, we just have an empty body
    sink.add_token(SyntaxKind::RBrace, rbrace_span);
    sink.finish_node(); // Finish FnBody

    sink.finish_node(); // Finish FnDeclaration
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    #[test]
    fn test_fn_basic() {
        let source = "fn foo() {}";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_fn_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FnDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("foo".to_string()));
        assert_eq!(decl.visibility(), None);
        assert_eq!(decl.return_type(), None);
        assert_eq!(decl.parameters().len(), 0);
    }

    #[test]
    fn test_fn_with_visibility() {
        let source = "public fn bar() {}";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_fn_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FnDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("bar".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
    }

    #[test]
    fn test_fn_with_return_type() {
        let source = "fn baz() -> Int {}";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_fn_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FnDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("baz".to_string()));
        assert_eq!(decl.return_type(), Some("Int".to_string()));
    }

    #[test]
    fn test_fn_with_parameters() {
        let source = "fn add(x: Int, y: Int) {}";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_fn_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FnDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("add".to_string()));
        let params = decl.parameters();
        assert_eq!(params.len(), 2);
        assert_eq!(params[0], ("x".to_string(), "Int".to_string()));
        assert_eq!(params[1], ("y".to_string(), "Int".to_string()));
    }

    #[test]
    fn test_fn_complete() {
        let source = "public fn multiply(a: Int, b: Int) -> Int {}";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_fn_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FnDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("multiply".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
        assert_eq!(decl.return_type(), Some("Int".to_string()));
        let params = decl.parameters();
        assert_eq!(params.len(), 2);
        assert_eq!(params[0], ("a".to_string(), "Int".to_string()));
        assert_eq!(params[1], ("b".to_string(), "Int".to_string()));
    }
}
