use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::visibility_parser_internal;
use crate::ty::{ty_parser, TyVariant};

/// Represents a function declaration: (visibility)? (static)? fn name(params) (-> return_type)? { }
///
/// The declaration is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl FunctionDeclaration {
    /// Create a new FunctionDeclaration from events and source text
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

    /// Check if this function has the static modifier
    pub fn is_static(&self) -> bool {
        self.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::StaticModifier)
    }

    /// Get the parameter list node
    pub fn parameter_list(&self) -> Option<SyntaxNode> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::ParameterList)
    }

    /// Get the return type node if present
    pub fn return_type(&self) -> Option<SyntaxNode> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::ReturnType)
    }

    /// Get the function body node
    pub fn body(&self) -> Option<SyntaxNode> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::FunctionBody)
    }
}

/// Raw parsed data for a single parameter
/// Parameter syntax: (label)? bind_name: Type
/// - label is an optional external parameter name (used by callers)
/// - bind_name is the internal parameter name (used in function body)
/// - If only one name is provided, it's used as both label and bind_name
#[derive(Debug, Clone)]
pub(crate) struct ParameterData {
    /// Optional label (external name for callers)
    /// If None, bind_name is used as the label
    pub label: Option<Span>,
    /// The binding name (internal name used in function body)
    pub bind_name: Span,
    /// The colon span
    pub colon: Span,
    /// The parameter type
    pub ty: TyVariant,
}

/// Raw parsed data for function declaration internals
#[derive(Debug, Clone)]
pub(crate) struct FunctionDeclarationData {
    pub visibility: Option<(Token, Span)>,
    pub is_static: Option<Span>,
    pub fn_span: Span,
    pub name_span: Span,
    pub lparen: Span,
    pub parameters: Vec<ParameterData>,
    pub rparen: Span,
    pub return_type: Option<(Span, TyVariant)>, // (arrow_span, return_ty)
    pub body: Option<(Span, Span)>,             // Optional (lbrace, rbrace) - None for protocol methods
}

/// Static modifier parser
fn static_parser_internal() -> impl Parser<Token, Option<Span>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(just(Token::Static).map_with_span(|_, span| Some(span)))
        .or(empty().map(|_| None))
}

/// Parser for a single parameter: (label)? bind_name: Type
///
/// Examples:
/// - `x: Int` -> label=None, bind_name=x
/// - `with x: Int` -> label=with, bind_name=x
fn parameter_parser_internal() -> impl Parser<Token, ParameterData, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    // Try to parse: identifier identifier : type (labeled parameter)
    // Or: identifier : type (unlabeled parameter)
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

/// Internal Chumsky parser for function declaration
/// Returns: FunctionDeclarationData
pub(crate) fn function_declaration_parser_internal() -> impl Parser<Token, FunctionDeclarationData, Error = Simple<Token>> + Clone {
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
            FunctionDeclarationData {
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

/// Parse a function declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_function_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match function_declaration_parser_internal().parse(stream) {
        Ok(data) => {
            emit_function_declaration(sink, data);
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

/// Emit events for a function declaration
fn emit_function_declaration(sink: &mut EventSink, data: FunctionDeclarationData) {
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

    // Emit FunctionBody node if present (optional for protocol methods)
    if let Some((lbrace, rbrace)) = data.body {
        sink.start_node(SyntaxKind::FunctionBody);
        sink.add_token(SyntaxKind::LBrace, lbrace);
        // TODO: Function body contents will be added later
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
    fn test_function_declaration_basic() {
        let source = "func test() { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_function_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FunctionDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("test".to_string()));
        assert_eq!(decl.visibility(), None);
        assert!(!decl.is_static());
    }

    #[test]
    fn test_function_declaration_with_visibility() {
        let source = "public func greet() { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_function_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FunctionDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("greet".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
    }

    #[test]
    fn test_function_declaration_static() {
        let source = "static func create() { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_function_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FunctionDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("create".to_string()));
        assert!(decl.is_static());
    }

    #[test]
    fn test_function_declaration_with_params() {
        let source = "func add(a: Int, b: Int) { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_function_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FunctionDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("add".to_string()));
        assert!(decl.parameter_list().is_some());
    }

    #[test]
    fn test_function_declaration_with_labeled_param() {
        let source = "func greet(with name: String) { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_function_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FunctionDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("greet".to_string()));
        assert!(decl.parameter_list().is_some());
    }

    #[test]
    fn test_function_declaration_with_return_type() {
        let source = "func multiply(x: Int, y: Int) -> Int { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_function_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FunctionDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("multiply".to_string()));
        assert!(decl.return_type().is_some());
    }

    #[test]
    fn test_function_declaration_full() {
        let source = "public static func calculate(value: Float, multiplier: Float) -> Float { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_function_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FunctionDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("calculate".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
        assert!(decl.is_static());
        assert!(decl.parameter_list().is_some());
        assert!(decl.return_type().is_some());
    }
}
