use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::visibility_parser_internal;
use crate::function::ParameterData;
use crate::ty::{ty_parser, TyVariant};

/// Represents a protocol declaration: (visibility)? protocol Name { ... }
///
/// The declaration is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolDeclaration {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl ProtocolDeclaration {
    /// Create a new ProtocolDeclaration from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the protocol name from this declaration
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

    /// Get the protocol body node
    pub fn body(&self) -> Option<SyntaxNode> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::ProtocolBody)
    }

    /// Get child function declarations (protocol methods)
    pub fn methods(&self) -> Vec<SyntaxNode> {
        self.body()
            .map(|body| {
                body.children()
                    .filter(|child| child.kind() == SyntaxKind::FunctionDeclaration)
                    .collect()
            })
            .unwrap_or_default()
    }
}

/// Raw parsed data for a function declaration inside a protocol body
/// Function: visibility, is_static, fn_span, name_span, lparen, parameters, rparen, return_type, body (optional)
#[derive(Debug, Clone)]
struct FunctionData {
    visibility: Option<(Token, Span)>,
    is_static: Option<Span>,
    fn_span: Span,
    name_span: Span,
    lparen: Span,
    parameters: Vec<ParameterData>,
    rparen: Span,
    return_type: Option<(Span, TyVariant)>,
    body: Option<(Span, Span)>,
}

/// Static modifier parser
fn static_parser_internal() -> impl Parser<Token, Option<Span>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(just(Token::Static).map_with_span(|_, span| Some(span)))
        .or(empty().map(|_| None))
}

/// Parser for a single parameter: (label)? bind_name: Type
fn parameter_parser_internal() -> impl Parser<Token, ParameterData, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(
            filter_map(|span, token| match token {
                Token::Identifier => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            })
        )
        .then(
            // Optional second identifier (if present, first was label)
            skip_trivia()
                .ignore_then(
                    filter_map(|span, token| match token {
                        Token::Identifier => Ok(span),
                        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
                    })
                )
                .or_not()
        )
        .then(
            skip_trivia()
                .ignore_then(just(Token::Colon).map_with_span(|_, span| span))
        )
        .then(ty_parser())
        .map(|(((first_ident, second_ident_opt), colon), ty)| {
            match second_ident_opt {
                Some(second_ident) => {
                    // Two identifiers: first is label, second is bind_name
                    ParameterData {
                        label: Some(first_ident),
                        bind_name: second_ident,
                        colon,
                        ty,
                    }
                }
                None => {
                    // One identifier: it's the bind_name, no label
                    ParameterData {
                        label: None,
                        bind_name: first_ident,
                        colon,
                        ty,
                    }
                }
            }
        })
}

/// Parser for parameter list (zero or more parameters separated by commas)
fn parameter_list_parser_internal() -> impl Parser<Token, Vec<ParameterData>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(
            parameter_parser_internal()
                .separated_by(just(Token::Comma).map_with_span(|_, span| span))
                .allow_trailing()
        )
}

/// Parser for optional return type: -> Type
fn return_type_parser_internal() -> impl Parser<Token, Option<(Span, TyVariant)>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(just(Token::Arrow).map_with_span(|_, span| span))
        .then(ty_parser())
        .map(|(arrow, ty)| (arrow, ty))
        .or_not()
}

/// Parser for optional function body: { }
fn function_body_parser_internal() -> impl Parser<Token, Option<(Span, Span)>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(just(Token::LBrace).map_with_span(|_, span| span))
        .then(
            skip_trivia()
                .ignore_then(just(Token::RBrace).map_with_span(|_, span| span))
        )
        .map(|(lbrace, rbrace)| Some((lbrace, rbrace)))
        .or(empty().map(|_| None))
}

/// Internal parser for a function declaration inside a protocol
fn function_parser_internal() -> impl Parser<Token, FunctionData, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    visibility_parser_internal()
        .then(static_parser_internal())
        .then(
            skip_trivia()
                .ignore_then(just(Token::Func).map_with_span(|_, span| span))
        )
        .then(
            skip_trivia()
                .ignore_then(filter_map(|span, token| match token {
                    Token::Identifier => Ok(span),
                    _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
                }))
        )
        .then(
            skip_trivia()
                .ignore_then(just(Token::LParen).map_with_span(|_, span| span))
        )
        .then(parameter_list_parser_internal())
        .then(
            skip_trivia()
                .ignore_then(just(Token::RParen).map_with_span(|_, span| span))
        )
        .then(return_type_parser_internal())
        .then(function_body_parser_internal())
        .map(|((((((((visibility, is_static), fn_span), name_span), lparen), parameters), rparen), return_type), body)| {
            FunctionData {
                visibility,
                is_static,
                fn_span,
                name_span,
                lparen,
                parameters,
                rparen,
                return_type,
                body,
            }
        })
}

/// Internal Chumsky parser for protocol declaration
/// Returns: (visibility, protocol_span, name_span, lbrace_span, body, rbrace_span)
fn protocol_declaration_parser_internal() -> impl Parser<
    Token,
    (Option<(Token, Span)>, Span, Span, Span, Vec<FunctionData>, Span),
    Error = Simple<Token>,
> + Clone {
    use crate::common::skip_trivia;

    visibility_parser_internal()
        .then(
            skip_trivia()
                .ignore_then(just(Token::Protocol).map_with_span(|_, span| span))
        )
        .then(
            skip_trivia()
                .ignore_then(filter_map(|span, token| match token {
                    Token::Identifier => Ok(span),
                    _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
                }))
        )
        .then(
            skip_trivia()
                .ignore_then(just(Token::LBrace).map_with_span(|_, span| span))
        )
        .then(function_parser_internal().repeated())
        .then(
            skip_trivia()
                .ignore_then(just(Token::RBrace).map_with_span(|_, span| span))
        )
        .map(
            |(((((visibility, protocol_span), name_span), lbrace_span), body), rbrace_span)| {
                (visibility, protocol_span, name_span, lbrace_span, body, rbrace_span)
            },
        )
}

/// Parse a protocol declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_protocol_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match protocol_declaration_parser_internal().parse(stream) {
        Ok((visibility, protocol_span, name_span, lbrace_span, body, rbrace_span)) => {
            emit_protocol_declaration(
                sink,
                visibility,
                protocol_span,
                name_span,
                lbrace_span,
                body,
                rbrace_span,
            );
        }
        Err(errors) => {
            // Emit error events for each parse error
            for error in errors {
                let span = error.span();
                sink.error_at(format!("Parse error: {:?}", error), span);
            }
        }
    }
}

/// Emit events for a protocol declaration
fn emit_protocol_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    protocol_span: Span,
    name_span: Span,
    lbrace_span: Span,
    body: Vec<FunctionData>,
    rbrace_span: Span,
) {
    sink.start_node(SyntaxKind::ProtocolDeclaration);

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

    sink.add_token(SyntaxKind::Protocol, protocol_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    // Emit ProtocolBody node wrapping the body content
    sink.start_node(SyntaxKind::ProtocolBody);
    sink.add_token(SyntaxKind::LBrace, lbrace_span);

    // Emit function declarations
    for func_data in body {
        emit_function_declaration(sink, func_data);
    }

    sink.add_token(SyntaxKind::RBrace, rbrace_span);
    sink.finish_node(); // Finish ProtocolBody

    sink.finish_node(); // Finish ProtocolDeclaration
}

/// Emit events for a function declaration
fn emit_function_declaration(sink: &mut EventSink, data: FunctionData) {
    sink.start_node(SyntaxKind::FunctionDeclaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
    if let Some((vis_token, vis_span)) = data.visibility {
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

    // Emit StaticModifier node if present
    if let Some(static_span) = data.is_static {
        sink.start_node(SyntaxKind::StaticModifier);
        sink.add_token(SyntaxKind::Static, static_span);
        sink.finish_node();
    }

    // Emit fn keyword
    sink.add_token(SyntaxKind::Func, data.fn_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, data.name_span);
    sink.finish_node(); // Finish Name

    // Emit ParameterList node
    emit_parameter_list(sink, data.lparen, data.parameters, data.rparen);

    // Emit ReturnType node if present
    if let Some((arrow_span, return_ty)) = data.return_type {
        emit_return_type(sink, arrow_span, return_ty);
    }

    // Emit FunctionBody node if present
    if let Some((lbrace, rbrace)) = data.body {
        sink.start_node(SyntaxKind::FunctionBody);
        sink.add_token(SyntaxKind::LBrace, lbrace);
        sink.add_token(SyntaxKind::RBrace, rbrace);
        sink.finish_node(); // Finish FunctionBody
    }

    sink.finish_node(); // Finish FunctionDeclaration
}

/// Emit events for a parameter list
fn emit_parameter_list(sink: &mut EventSink, lparen: Span, parameters: Vec<ParameterData>, rparen: Span) {
    sink.start_node(SyntaxKind::ParameterList);
    sink.add_token(SyntaxKind::LParen, lparen);

    for param in parameters {
        emit_parameter(sink, param);
    }

    sink.add_token(SyntaxKind::RParen, rparen);
    sink.finish_node(); // Finish ParameterList
}

/// Emit events for a single parameter
fn emit_parameter(sink: &mut EventSink, param: ParameterData) {
    use crate::ty::emit_ty_variant;

    sink.start_node(SyntaxKind::Parameter);

    // Emit label if present (as a Name node)
    if let Some(label_span) = param.label {
        sink.start_node(SyntaxKind::Name);
        sink.add_token(SyntaxKind::Identifier, label_span);
        sink.finish_node();
    }

    // Emit bind_name (as a Name node)
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, param.bind_name);
    sink.finish_node();

    sink.add_token(SyntaxKind::Colon, param.colon);

    // Emit the type
    emit_ty_variant(sink, &param.ty);

    sink.finish_node(); // Finish Parameter
}

/// Emit events for a return type
fn emit_return_type(sink: &mut EventSink, arrow_span: Span, return_ty: TyVariant) {
    use crate::ty::emit_ty_variant;

    sink.start_node(SyntaxKind::ReturnType);
    sink.add_token(SyntaxKind::Arrow, arrow_span);

    // Emit the return type
    emit_ty_variant(sink, &return_ty);

    sink.finish_node(); // Finish ReturnType
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    #[test]
    fn test_protocol_declaration_basic() {
        let source = "protocol Drawable { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_protocol_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ProtocolDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Drawable".to_string()));
        assert_eq!(decl.visibility(), None);
        assert_eq!(decl.syntax.kind(), SyntaxKind::ProtocolDeclaration);
    }

    #[test]
    fn test_protocol_declaration_with_visibility() {
        let source = "public protocol Serializable { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_protocol_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ProtocolDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Serializable".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
    }

    #[test]
    fn test_protocol_with_method() {
        let source = "protocol Drawable { func draw() }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_protocol_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ProtocolDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Drawable".to_string()));
        let methods = decl.methods();
        assert_eq!(methods.len(), 1);
    }

    #[test]
    fn test_protocol_with_method_and_return_type() {
        let source = "protocol Serializable { func serialize() -> String }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_protocol_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ProtocolDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Serializable".to_string()));
        let methods = decl.methods();
        assert_eq!(methods.len(), 1);
    }

    #[test]
    fn test_protocol_with_multiple_methods() {
        let source = "protocol Collection { func count() -> Int func isEmpty() -> Bool }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_protocol_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ProtocolDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Collection".to_string()));
        let methods = decl.methods();
        assert_eq!(methods.len(), 2);
    }

    #[test]
    fn test_protocol_method_with_parameters() {
        let source = "protocol NetworkClient { func fetch(from url: String) -> Data }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_protocol_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ProtocolDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("NetworkClient".to_string()));
        let methods = decl.methods();
        assert_eq!(methods.len(), 1);
    }

    #[test]
    fn test_protocol_method_with_body_parses() {
        // This should parse successfully - the error about having a body
        // should be detected at semantic analysis, not parsing
        let source = "protocol BadProtocol { func doSomething() { } }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_protocol_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ProtocolDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("BadProtocol".to_string()));
        let methods = decl.methods();
        assert_eq!(methods.len(), 1);

        // The method should have a FunctionBody node
        let method = &methods[0];
        let has_body = method.children().any(|c| c.kind() == SyntaxKind::FunctionBody);
        assert!(has_body, "Protocol method with body should parse and include FunctionBody node");
    }
}
