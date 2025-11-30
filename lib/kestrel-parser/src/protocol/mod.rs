//! Protocol declaration parsing
//!
//! This module is the single source of truth for protocol declaration parsing.
//! Protocol bodies can only contain function declarations (methods).

use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::{
    visibility_parser_internal, token, identifier,
    function_declaration_parser_internal,
    emit_protocol_declaration,
    ProtocolDeclarationData,
};
use crate::type_param::{type_parameter_list_parser, where_clause_parser, conformance_list_parser};
use crate::common::ConformanceListData;

/// Represents a protocol declaration: (visibility)? protocol Name[T]? (where ...)? { ... }
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

/// Internal Chumsky parser for protocol declaration
///
/// This is the single source of truth for protocol declaration parsing.
pub fn protocol_declaration_parser_internal() -> impl Parser<Token, ProtocolDeclarationData, Error = Simple<Token>> + Clone {
    visibility_parser_internal()
        .then(token(Token::Protocol))
        .then(identifier())
        .then(type_parameter_list_parser().or_not())
        .then(conformance_list_parser().or_not())
        .then(where_clause_parser().or_not())
        .then(token(Token::LBrace))
        .then(function_declaration_parser_internal().repeated())
        .then(token(Token::RBrace))
        .map(|((((((((visibility, protocol_span), name_span), type_params), inherited), where_clause), lbrace_span), body), rbrace_span)| {
            ProtocolDeclarationData {
                visibility,
                protocol_span,
                name_span,
                type_params,
                inherited: inherited.map(|(colon_span, types)| ConformanceListData {
                    colon_span,
                    conformances: types,
                }),
                where_clause,
                lbrace_span,
                body,
                rbrace_span,
            }
        })
}

/// Parse a protocol declaration and emit events
///
/// This is the primary event-driven parser function for protocol declarations.
pub fn parse_protocol_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match protocol_declaration_parser_internal().parse(stream) {
        Ok(data) => {
            emit_protocol_declaration(sink, data);
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
    fn test_protocol_with_type_params() {
        let source = "protocol Collection[T] { }";
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
        let has_type_params = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::TypeParameterList);
        assert!(has_type_params, "Expected TypeParameterList node");
    }

    #[test]
    fn test_protocol_with_where_clause() {
        let source = "protocol Comparable[T] where T: Equatable { }";
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

        assert_eq!(decl.name(), Some("Comparable".to_string()));
        let has_where_clause = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::WhereClause);
        assert!(has_where_clause, "Expected WhereClause node");
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
    fn test_protocol_method_with_generics() {
        let source = "protocol Container { func get[T](index: Int) -> T }";
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

        assert_eq!(decl.name(), Some("Container".to_string()));
        let methods = decl.methods();
        assert_eq!(methods.len(), 1);

        // Check that the method has type parameters
        let method = &methods[0];
        let has_type_params = method.children()
            .any(|child| child.kind() == SyntaxKind::TypeParameterList);
        assert!(has_type_params, "Expected TypeParameterList on method");
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

    #[test]
    fn test_protocol_inheritance() {
        let source = "protocol Shape: Drawable { }";
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

        assert_eq!(decl.name(), Some("Shape".to_string()));
        let has_conformance = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::ConformanceList);
        assert!(has_conformance, "Expected ConformanceList node for protocol inheritance");
    }

    #[test]
    fn test_protocol_multiple_inheritance() {
        let source = "protocol Widget: Drawable, Clickable { }";
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

        let conformance_list = decl.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::ConformanceList)
            .expect("Expected ConformanceList node");

        let conformance_count = conformance_list
            .children()
            .filter(|c| c.kind() == SyntaxKind::ConformanceItem)
            .count();
        assert_eq!(conformance_count, 2);
    }
}
