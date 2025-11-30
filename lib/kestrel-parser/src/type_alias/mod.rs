//! Type alias declaration parsing
//!
//! This module is the single source of truth for type alias declaration parsing.
//! Type aliases support generics with type parameters.

use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::{
    visibility_parser_internal, token, identifier,
    emit_type_alias_declaration,
    TypeAliasDeclarationData,
};
use crate::ty::ty_parser;
use crate::type_param::type_parameter_list_parser;

/// Represents a type alias declaration: (visibility)? type Name[T]? = Type;
///
/// The declaration is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAliasDeclaration {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl TypeAliasDeclaration {
    /// Create a new TypeAliasDeclaration from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the alias name from this declaration
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

    /// Check if this type alias has type parameters
    pub fn has_type_parameters(&self) -> bool {
        self.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::TypeParameterList)
    }

    /// Get the aliased type name from this declaration
    /// Note: This is a best-effort helper for simple path types.
    pub fn aliased_type(&self) -> Option<String> {
        let aliased_node = self
            .syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::AliasedType)?;

        let ty_node = aliased_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Ty)?;

        let ty_path_node = ty_node
            .children()
            .find(|child| child.kind() == SyntaxKind::TyPath)?;

        let path_node = ty_path_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Path)?;

        let segments: Vec<String> = path_node
            .children()
            .filter(|child| child.kind() == SyntaxKind::PathElement)
            .filter_map(|elem| {
                elem.children_with_tokens()
                    .filter_map(|t| t.into_token())
                    .find(|tok| tok.kind() == SyntaxKind::Identifier)
                    .map(|tok| tok.text().to_string())
            })
            .collect();

        if segments.is_empty() {
            None
        } else {
            Some(segments.join("."))
        }
    }
}

/// Internal Chumsky parser for type alias declaration
///
/// This is the single source of truth for type alias declaration parsing.
pub fn type_alias_declaration_parser_internal() -> impl Parser<Token, TypeAliasDeclarationData, Error = Simple<Token>> + Clone {
    visibility_parser_internal()
        .then(token(Token::Type))
        .then(identifier())
        .then(type_parameter_list_parser().or_not())
        .then(token(Token::Equals))
        .then(ty_parser())
        .then(token(Token::Semicolon))
        .map(|((((((visibility, type_span), name_span), type_params), equals_span), aliased_type), semicolon_span)| {
            TypeAliasDeclarationData {
                visibility,
                type_span,
                name_span,
                type_params,
                equals_span,
                aliased_type,
                semicolon_span,
            }
        })
}

/// Parse a type alias declaration and emit events
///
/// This is the primary event-driven parser function for type alias declarations.
pub fn parse_type_alias_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match type_alias_declaration_parser_internal().parse(stream) {
        Ok(data) => {
            emit_type_alias_declaration(sink, data);
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
    fn test_type_alias_declaration_basic() {
        let source = "type Alias = Aliased;";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_type_alias_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = TypeAliasDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Alias".to_string()));
        assert_eq!(decl.aliased_type(), Some("Aliased".to_string()));
        assert_eq!(decl.visibility(), None);
    }

    #[test]
    fn test_type_alias_declaration_with_visibility() {
        let source = "public type PublicAlias = SomeType;";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_type_alias_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = TypeAliasDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("PublicAlias".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
    }

    #[test]
    fn test_type_alias_declaration_with_generics() {
        let source = "type Box[T] = T;";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_type_alias_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = TypeAliasDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Box".to_string()));
        assert!(decl.has_type_parameters());
    }

    #[test]
    fn test_type_alias_declaration_tuple() {
        let source = "type TupleAlias = (Int, String);";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_type_alias_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = TypeAliasDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("TupleAlias".to_string()));
        // aliased_type() only works for simple paths
        assert_eq!(decl.aliased_type(), None);

        // Verify structure
        let aliased = decl.syntax.children().find(|c| c.kind() == SyntaxKind::AliasedType).unwrap();
        let ty = aliased.children().find(|c| c.kind() == SyntaxKind::Ty).unwrap();
        assert_eq!(ty.children().next().unwrap().kind(), SyntaxKind::TyTuple);
    }
}
