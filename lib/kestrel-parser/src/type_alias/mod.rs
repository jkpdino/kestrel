use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::visibility_parser_internal;
use crate::ty::{
    emit_function_type, emit_never_type, emit_path_type, emit_tuple_type, emit_unit_type,
    ty_parser, TyVariant,
};

/// Represents a type alias declaration: (visibility)? type Alias = Aliased;
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

    /// Get the aliased type name from this declaration
    /// Note: This is a best-effort helper for simple path types.
    /// For complex types (tuples, functions), this might not return a simple string.
    pub fn aliased_type(&self) -> Option<String> {
        let aliased_node = self
            .syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::AliasedType)?;
            
        // Try to find a Ty node
        let ty_node = aliased_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Ty)?;

        // Check if it's a TyPath
        let ty_path_node = ty_node
            .children()
            .find(|child| child.kind() == SyntaxKind::TyPath)?;
            
        let path_node = ty_path_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Path)?;
            
        // Reconstruct path string from Path elements
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
/// Returns: (visibility, type_span, name_span, equals_span, aliased_type_variant, semicolon_span)
fn type_alias_declaration_parser_internal() -> impl Parser<
    Token,
    (
        Option<(Token, Span)>,
        Span,
        Span,
        Span,
        TyVariant,
        Span,
    ),
    Error = Simple<Token>,
> + Clone {
    visibility_parser_internal()
        .then(just(Token::Type).map_with_span(|_, span| span))
        .then(filter_map(|span, token| match token {
            Token::Identifier => Ok(span),
            _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
        }))
        .then(just(Token::Equals).map_with_span(|_, span| span))
        .then(ty_parser())
        .then(just(Token::Semicolon).map_with_span(|_, span| span))
        .map(
            |(((((visibility, type_span), name_span), equals_span), aliased_type_variant), semicolon_span)| {
                (
                    visibility,
                    type_span,
                    name_span,
                    equals_span,
                    aliased_type_variant,
                    semicolon_span,
                )
            },
        )
}

/// Parse a type alias declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_type_alias_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match type_alias_declaration_parser_internal().parse(stream) {
        Ok((
            visibility,
            type_span,
            name_span,
            equals_span,
            aliased_type_variant,
            semicolon_span,
        )) => {
            emit_type_alias_declaration(
                sink,
                visibility,
                type_span,
                name_span,
                equals_span,
                aliased_type_variant,
                semicolon_span,
            );
        }
        Err(errors) => {
            // Emit error events for each parse error
            for error in errors {
                // Chumsky errors have span information
                let span = error.span();
                sink.error_at(format!("Parse error: {:?}", error), span);
            }
        }
    }
}

/// Emit events for a type alias declaration
/// Internal helper function
fn emit_type_alias_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    type_span: Span,
    name_span: Span,
    equals_span: Span,
    aliased_type_variant: TyVariant,
    semicolon_span: Span,
) {
    sink.start_node(SyntaxKind::TypeAliasDeclaration);

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

    sink.add_token(SyntaxKind::Type, type_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    sink.add_token(SyntaxKind::Equals, equals_span);

    // Emit AliasedType node wrapping the aliased type
    sink.start_node(SyntaxKind::AliasedType);
    
    // Delegate to ty module emitters
    match aliased_type_variant {
        TyVariant::Unit(lparen, rparen) => emit_unit_type(sink, lparen, rparen),
        TyVariant::Never(bang) => emit_never_type(sink, bang),
        TyVariant::Tuple(lparen, types, rparen) => emit_tuple_type(sink, lparen, types, rparen),
        TyVariant::Function(lparen, params, rparen, arrow, ret) => {
            emit_function_type(sink, lparen, params, rparen, arrow, ret)
        }
        TyVariant::Path(segments) => emit_path_type(sink, &segments),
    }
    
    sink.finish_node(); // Finish AliasedType

    sink.add_token(SyntaxKind::Semicolon, semicolon_span);

    sink.finish_node(); // Finish TypeAliasDeclaration
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
        assert_eq!(decl.syntax.kind(), SyntaxKind::TypeAliasDeclaration);
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
        assert_eq!(decl.aliased_type(), Some("SomeType".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
        assert_eq!(decl.syntax.kind(), SyntaxKind::TypeAliasDeclaration);
    }

    #[test]
    fn test_type_alias_declaration_private() {
        let source = "private type PrivateAlias = HiddenType;";
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

        assert_eq!(decl.name(), Some("PrivateAlias".to_string()));
        assert_eq!(decl.aliased_type(), Some("HiddenType".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Private));
        assert_eq!(decl.syntax.kind(), SyntaxKind::TypeAliasDeclaration);
    }

    #[test]
    fn test_type_alias_declaration_internal() {
        let source = "internal type InternalAlias = ModuleType;";
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

        assert_eq!(decl.name(), Some("InternalAlias".to_string()));
        assert_eq!(decl.aliased_type(), Some("ModuleType".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Internal));
        assert_eq!(decl.syntax.kind(), SyntaxKind::TypeAliasDeclaration);
    }

    #[test]
    fn test_type_alias_declaration_fileprivate() {
        let source = "fileprivate type FileAlias = LocalType;";
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

        assert_eq!(decl.name(), Some("FileAlias".to_string()));
        assert_eq!(decl.aliased_type(), Some("LocalType".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Fileprivate));
        assert_eq!(decl.syntax.kind(), SyntaxKind::TypeAliasDeclaration);
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
        // aliased_type() helper currently only works for simple paths, so it returns None for Tuple
        // In a real scenario, we would check the structure of the tree
        assert_eq!(decl.aliased_type(), None);
        
        // Verify structure manually
        let aliased = decl.syntax.children().find(|c| c.kind() == SyntaxKind::AliasedType).unwrap();
        let ty = aliased.children().find(|c| c.kind() == SyntaxKind::Ty).unwrap();
        assert_eq!(ty.children().next().unwrap().kind(), SyntaxKind::TyTuple);
    }
}
