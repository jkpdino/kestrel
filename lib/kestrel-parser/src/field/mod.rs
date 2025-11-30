use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::visibility_parser_internal;
use crate::ty::{ty_parser, TyVariant};

/// Represents a field declaration: (visibility)? (static)? let/var name: Type
///
/// The declaration is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldDeclaration {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl FieldDeclaration {
    /// Create a new FieldDeclaration from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the field name from this declaration
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

    /// Check if this field has the static modifier
    pub fn is_static(&self) -> bool {
        self.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::StaticModifier)
    }

    /// Check if this field is mutable (var vs let)
    pub fn is_mutable(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .any(|tok| tok.kind() == SyntaxKind::Var)
    }

    /// Get the type expression
    pub fn ty(&self) -> Option<SyntaxNode> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::Ty)
    }
}

/// Raw parsed data for field declaration internals
#[derive(Debug, Clone)]
struct FieldDeclarationData {
    visibility: Option<(Token, Span)>,
    is_static: Option<Span>,
    mutability_span: Span,
    is_mutable: bool,
    name_span: Span,
    colon_span: Span,
    ty: TyVariant,
}

/// Static modifier parser
fn static_parser_internal() -> impl Parser<Token, Option<Span>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(just(Token::Static).map_with_span(|_, span| Some(span)))
        .or(empty().map(|_| None))
}

/// Let/var parser - returns (span, is_mutable)
fn let_var_parser_internal() -> impl Parser<Token, (Span, bool), Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(
            just(Token::Let)
                .map_with_span(|_, span| (span, false))
                .or(just(Token::Var).map_with_span(|_, span| (span, true)))
        )
}

/// Internal Chumsky parser for field declaration
/// Returns: FieldDeclarationData
fn field_declaration_parser_internal() -> impl Parser<Token, FieldDeclarationData, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    visibility_parser_internal()
        .then(static_parser_internal())
        .then(let_var_parser_internal())
        .then(skip_trivia().ignore_then(filter_map(|span, token| match token {
            Token::Identifier => Ok(span),
            _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
        })))
        .then(skip_trivia().ignore_then(just(Token::Colon).map_with_span(|_, span| span)))
        .then(ty_parser())
        .map(|(((((visibility, is_static), (mutability_span, is_mutable)), name_span), colon_span), ty)| {
            FieldDeclarationData {
                visibility,
                is_static,
                mutability_span,
                is_mutable,
                name_span,
                colon_span,
                ty,
            }
        })
}

/// Parse a field declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_field_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match field_declaration_parser_internal().parse(stream) {
        Ok(data) => {
            emit_field_declaration(sink, data);
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

/// Emit events for a field declaration
fn emit_field_declaration(sink: &mut EventSink, data: FieldDeclarationData) {
    sink.start_node(SyntaxKind::FieldDeclaration);

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

    // Emit let or var keyword
    if data.is_mutable {
        sink.add_token(SyntaxKind::Var, data.mutability_span);
    } else {
        sink.add_token(SyntaxKind::Let, data.mutability_span);
    }

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, data.name_span);
    sink.finish_node(); // Finish Name

    sink.add_token(SyntaxKind::Colon, data.colon_span);

    // Emit the type
    crate::ty::emit_ty_variant(sink, &data.ty);

    sink.finish_node(); // Finish FieldDeclaration
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    #[test]
    fn test_field_declaration_basic() {
        let source = "let x: Int";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_field_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FieldDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("x".to_string()));
        assert_eq!(decl.visibility(), None);
        assert!(!decl.is_static());
        assert!(!decl.is_mutable());
    }

    #[test]
    fn test_field_declaration_var() {
        let source = "var count: Int";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_field_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FieldDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("count".to_string()));
        assert!(decl.is_mutable());
    }

    #[test]
    fn test_field_declaration_static() {
        let source = "static let instance: Self";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_field_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FieldDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("instance".to_string()));
        assert!(decl.is_static());
        assert!(!decl.is_mutable());
    }

    #[test]
    fn test_field_declaration_with_visibility() {
        let source = "public let name: String";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_field_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FieldDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("name".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
    }

    #[test]
    fn test_field_declaration_full() {
        let source = "public static var counter: Int";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_field_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = FieldDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("counter".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
        assert!(decl.is_static());
        assert!(decl.is_mutable());
    }
}
