use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::{
    module_declaration_parser_internal,
    visibility_parser_internal, import_declaration_parser_internal,
    emit_module_path, emit_import_declaration,
};
use crate::type_param::{
    TypeParameterData, WhereClauseData,
    type_parameter_list_parser, where_clause_parser,
    emit_type_parameter_list, emit_where_clause,
};

/// Represents a struct declaration: (visibility)? struct Name { ... }
///
/// The declaration is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDeclaration {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl StructDeclaration {
    /// Create a new StructDeclaration from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the struct name from this declaration
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

    /// Get child declaration items (nested structs, imports, modules)
    pub fn children(&self) -> Vec<SyntaxNode> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::StructBody)
            .map(|body| {
                body.children()
                    .filter(|child| {
                        matches!(
                            child.kind(),
                            SyntaxKind::StructDeclaration
                                | SyntaxKind::ImportDeclaration
                                | SyntaxKind::ModuleDeclaration
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    }
}

/// Raw parsed data for a declaration item inside a struct body
#[derive(Debug, Clone)]
enum DeclarationItemData {
    Module(Span, Vec<Span>),
    Import(Span, Vec<Span>, Option<Span>, Option<Vec<(Span, Option<Span>)>>),
    Struct {
        visibility: Option<(Token, Span)>,
        struct_span: Span,
        name_span: Span,
        type_params: Option<(Span, Vec<TypeParameterData>, Span)>,
        where_clause: Option<WhereClauseData>,
        lbrace_span: Span,
        body: Vec<DeclarationItemData>,
        rbrace_span: Span,
    },
}

/// Internal parser for a single declaration item
fn declaration_item_parser_internal() -> impl Parser<Token, DeclarationItemData, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    recursive(|declaration_item| {
        let module_parser = module_declaration_parser_internal()
            .map(|(span, path)| DeclarationItemData::Module(span, path));

        let import_parser = import_declaration_parser_internal()
            .map(|(import_span, path, alias, items)| DeclarationItemData::Import(import_span, path, alias, items));

        let struct_parser = visibility_parser_internal()
            .then(skip_trivia().ignore_then(just(Token::Struct).map_with_span(|_, span| span)))
            .then(skip_trivia().ignore_then(filter_map(|span, token| match token {
                Token::Identifier => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            })))
            .then(type_parameter_list_parser().or_not())
            .then(where_clause_parser().or_not())
            .then(skip_trivia().ignore_then(just(Token::LBrace).map_with_span(|_, span| span)))
            .then(declaration_item.repeated())
            .then(skip_trivia().ignore_then(just(Token::RBrace).map_with_span(|_, span| span)))
            .map(|(((((((visibility, struct_span), name_span), type_params), where_clause), lbrace_span), body), rbrace_span)| {
                DeclarationItemData::Struct {
                    visibility,
                    struct_span,
                    name_span,
                    type_params,
                    where_clause,
                    lbrace_span,
                    body,
                    rbrace_span,
                }
            });

        module_parser.or(import_parser).or(struct_parser)
    })
}

/// Parsed struct declaration data
#[derive(Debug, Clone)]
struct StructDeclarationData {
    visibility: Option<(Token, Span)>,
    struct_span: Span,
    name_span: Span,
    type_params: Option<(Span, Vec<TypeParameterData>, Span)>,
    where_clause: Option<WhereClauseData>,
    lbrace_span: Span,
    body: Vec<DeclarationItemData>,
    rbrace_span: Span,
}

/// Internal Chumsky parser for struct declaration
fn struct_declaration_parser_internal() -> impl Parser<Token, StructDeclarationData, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    visibility_parser_internal()
        .then(skip_trivia().ignore_then(just(Token::Struct).map_with_span(|_, span| span)))
        .then(skip_trivia().ignore_then(filter_map(|span, token| match token {
            Token::Identifier => Ok(span),
            _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
        })))
        .then(type_parameter_list_parser().or_not())
        .then(where_clause_parser().or_not())
        .then(skip_trivia().ignore_then(just(Token::LBrace).map_with_span(|_, span| span)))
        .then(declaration_item_parser_internal().repeated())
        .then(skip_trivia().ignore_then(just(Token::RBrace).map_with_span(|_, span| span)))
        .map(
            |(((((((visibility, struct_span), name_span), type_params), where_clause), lbrace_span), body), rbrace_span)| {
                StructDeclarationData {
                    visibility,
                    struct_span,
                    name_span,
                    type_params,
                    where_clause,
                    lbrace_span,
                    body,
                    rbrace_span,
                }
            },
        )
}

/// Parse a struct declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_struct_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match struct_declaration_parser_internal().parse(stream) {
        Ok(data) => {
            emit_struct_declaration_data(sink, data);
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

/// Emit events for a struct declaration from StructDeclarationData
fn emit_struct_declaration_data(sink: &mut EventSink, data: StructDeclarationData) {
    sink.start_node(SyntaxKind::StructDeclaration);

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

    sink.add_token(SyntaxKind::Struct, data.struct_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, data.name_span);
    sink.finish_node(); // Finish Name

    // Emit TypeParameterList if present
    if let Some((lbracket, params, rbracket)) = data.type_params {
        emit_type_parameter_list(sink, lbracket, params, rbracket);
    }

    // Emit WhereClause if present
    if let Some(where_clause) = data.where_clause {
        emit_where_clause(sink, where_clause);
    }

    // Emit StructBody node wrapping the body content
    sink.start_node(SyntaxKind::StructBody);
    sink.add_token(SyntaxKind::LBrace, data.lbrace_span);

    // Emit nested declaration items
    for item_data in data.body {
        emit_declaration_item(sink, item_data);
    }

    sink.add_token(SyntaxKind::RBrace, data.rbrace_span);
    sink.finish_node(); // Finish StructBody

    sink.finish_node(); // Finish StructDeclaration
}

/// Emit events for a declaration item
/// Helper function used by emit_struct_declaration_data
fn emit_declaration_item(sink: &mut EventSink, item_data: DeclarationItemData) {
    match item_data {
        DeclarationItemData::Module(module_span, path_segments) => {
            sink.start_node(SyntaxKind::ModuleDeclaration);
            sink.add_token(SyntaxKind::Module, module_span);
            emit_module_path(sink, &path_segments);
            sink.finish_node();
        }
        DeclarationItemData::Import(import_span, path_segments, alias, items) => {
            emit_import_declaration(sink, import_span, &path_segments, alias, items);
        }
        DeclarationItemData::Struct {
            visibility,
            struct_span,
            name_span,
            type_params,
            where_clause,
            lbrace_span,
            body,
            rbrace_span,
        } => {
            emit_struct_declaration_data(sink, StructDeclarationData {
                visibility,
                struct_span,
                name_span,
                type_params,
                where_clause,
                lbrace_span,
                body,
                rbrace_span,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    #[test]
    fn test_struct_declaration_basic() {
        let source = "struct Foo { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_struct_declaration(source, tokens.into_iter(), &mut sink);

        // Debug: print events
        for (i, event) in sink.events().iter().enumerate() {
            eprintln!("{}: {:?}", i, event);
        }

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = StructDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Foo".to_string()));
        assert_eq!(decl.visibility(), None);
        assert_eq!(decl.syntax.kind(), SyntaxKind::StructDeclaration);
    }

    #[test]
    fn test_struct_declaration_with_visibility() {
        let source = "public struct Bar { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_struct_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = StructDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Bar".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Public));
        assert_eq!(decl.syntax.kind(), SyntaxKind::StructDeclaration);
    }

    #[test]
    fn test_struct_declaration_private() {
        let source = "private struct Baz { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_struct_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = StructDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Baz".to_string()));
        assert_eq!(decl.visibility(), Some(SyntaxKind::Private));
    }

    #[test]
    fn test_struct_declaration_with_nested_struct() {
        let source = "struct Outer { struct Inner { } }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_struct_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = StructDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Outer".to_string()));
        assert_eq!(decl.visibility(), None);

        // Check that there is one nested struct
        let children = decl.children();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].kind(), SyntaxKind::StructDeclaration);

        // Check the nested struct name using the Name node
        let nested_name = children[0]
            .children()
            .find(|n| n.kind() == SyntaxKind::Name)
            .and_then(|name_node| {
                name_node
                    .children_with_tokens()
                    .filter_map(|elem| elem.into_token())
                    .find(|tok| tok.kind() == SyntaxKind::Identifier)
                    .map(|tok| tok.text().to_string())
            });
        assert_eq!(nested_name, Some("Inner".to_string()));
    }

    #[test]
    fn test_struct_declaration_with_type_params() {
        let source = "struct Box[T] { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_struct_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = StructDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Box".to_string()));
        assert_eq!(decl.syntax.kind(), SyntaxKind::StructDeclaration);

        // Check that TypeParameterList exists
        let has_type_params = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::TypeParameterList);
        assert!(has_type_params, "Expected TypeParameterList node");
    }

    #[test]
    fn test_struct_declaration_with_multiple_type_params() {
        let source = "struct Pair[A, B] { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_struct_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = StructDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Pair".to_string()));

        // Check TypeParameterList has multiple TypeParameter children
        let type_param_list = decl.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::TypeParameterList);
        assert!(type_param_list.is_some(), "Expected TypeParameterList node");

        let type_params_count = type_param_list.unwrap()
            .children()
            .filter(|child| child.kind() == SyntaxKind::TypeParameter)
            .count();
        assert_eq!(type_params_count, 2);
    }

    #[test]
    fn test_struct_declaration_with_where_clause() {
        let source = "struct Set[T] where T: Equatable { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_struct_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = StructDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.name(), Some("Set".to_string()));

        // Check that WhereClause exists
        let has_where_clause = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::WhereClause);
        assert!(has_where_clause, "Expected WhereClause node");
    }
}
