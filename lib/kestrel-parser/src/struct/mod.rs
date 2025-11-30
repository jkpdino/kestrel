//! Struct declaration parsing
//!
//! This module is the single source of truth for struct declaration parsing.
//! Struct bodies can contain: fields, functions, nested structs, modules, and imports.

use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};
use crate::common::{
    visibility_parser_internal, token, identifier,
    module_declaration_parser_internal, import_declaration_parser_internal,
    function_declaration_parser_internal, field_declaration_parser_internal,
    emit_struct_declaration,
    StructDeclarationData, StructBodyItem,
};
use crate::type_param::{type_parameter_list_parser, where_clause_parser, conformance_list_parser};
use crate::common::ConformanceListData;

/// Represents a struct declaration: (visibility)? struct Name[T]? (where ...)? { ... }
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

    /// Get child declaration items (nested structs, imports, modules, fields, functions)
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
                                | SyntaxKind::FieldDeclaration
                                | SyntaxKind::FunctionDeclaration
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    }
}

/// Internal parser for struct body items
///
/// Struct bodies can contain: fields, functions, nested structs, modules, and imports.
fn struct_body_item_parser_internal(
    struct_parser: impl Parser<Token, StructDeclarationData, Error = Simple<Token>> + Clone
) -> impl Parser<Token, StructBodyItem, Error = Simple<Token>> + Clone {
    // Module declaration
    let module_parser = module_declaration_parser_internal()
        .map(|(module_span, path)| StructBodyItem::Module(module_span, path));

    // Import declaration
    let import_parser = import_declaration_parser_internal()
        .map(|(import_span, path, alias, items)| StructBodyItem::Import(import_span, path, alias, items));

    // Nested struct declaration
    let nested_struct_parser = struct_parser
        .map(StructBodyItem::Struct);

    // Function declaration
    let function_parser = function_declaration_parser_internal()
        .map(StructBodyItem::Function);

    // Field declaration
    let field_parser = field_declaration_parser_internal()
        .map(StructBodyItem::Field);

    // Try each parser - order matters for disambiguation
    // Put more specific parsers first
    module_parser
        .or(import_parser)
        .or(nested_struct_parser)
        .or(function_parser)
        .or(field_parser)
}

/// Internal Chumsky parser for struct declaration
///
/// This is the single source of truth for struct declaration parsing.
pub fn struct_declaration_parser_internal() -> impl Parser<Token, StructDeclarationData, Error = Simple<Token>> + Clone {
    recursive(|struct_parser| {
        visibility_parser_internal()
            .then(token(Token::Struct))
            .then(identifier())
            .then(type_parameter_list_parser().or_not())
            .then(conformance_list_parser().or_not())
            .then(where_clause_parser().or_not())
            .then(token(Token::LBrace))
            .then(struct_body_item_parser_internal(struct_parser).repeated())
            .then(token(Token::RBrace))
            .map(|((((((((visibility, struct_span), name_span), type_params), conformances), where_clause), lbrace_span), body), rbrace_span)| {
                StructDeclarationData {
                    visibility,
                    struct_span,
                    name_span,
                    type_params,
                    conformances: conformances.map(|(colon_span, types)| ConformanceListData {
                        colon_span,
                        conformances: types,
                    }),
                    where_clause,
                    lbrace_span,
                    body,
                    rbrace_span,
                }
            })
    })
}

/// Parse a struct declaration and emit events
///
/// This is the primary event-driven parser function for struct declarations.
pub fn parse_struct_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match struct_declaration_parser_internal().parse(stream) {
        Ok(data) => {
            emit_struct_declaration(sink, data);
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
    fn test_struct_declaration_basic() {
        let source = "struct Foo { }";
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
        let children = decl.children();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].kind(), SyntaxKind::StructDeclaration);
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
        let has_type_params = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::TypeParameterList);
        assert!(has_type_params, "Expected TypeParameterList node");
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
        let has_where_clause = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::WhereClause);
        assert!(has_where_clause, "Expected WhereClause node");
    }

    #[test]
    fn test_struct_with_field() {
        let source = "struct Point { let x: Int }";
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

        let children = decl.children();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].kind(), SyntaxKind::FieldDeclaration);
    }

    #[test]
    fn test_struct_with_function() {
        let source = "struct Calculator { func add(a: Int, b: Int) -> Int { } }";
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

        let children = decl.children();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].kind(), SyntaxKind::FunctionDeclaration);
    }

    #[test]
    fn test_struct_with_multiple_members() {
        let source = "struct Person { let name: String var age: Int func greet() { } }";
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

        let children = decl.children();
        assert_eq!(children.len(), 3);
        assert_eq!(children[0].kind(), SyntaxKind::FieldDeclaration);
        assert_eq!(children[1].kind(), SyntaxKind::FieldDeclaration);
        assert_eq!(children[2].kind(), SyntaxKind::FunctionDeclaration);
    }

    #[test]
    fn test_struct_with_conformance() {
        let source = "struct Point: Drawable { }";
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

        assert_eq!(decl.name(), Some("Point".to_string()));
        let has_conformance = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::ConformanceList);
        assert!(has_conformance, "Expected ConformanceList node");
    }

    #[test]
    fn test_struct_with_multiple_conformances() {
        let source = "struct Point: Drawable, Equatable { }";
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

    #[test]
    fn test_struct_with_generic_conformance() {
        let source = "struct IntBox: Container[Int] { }";
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

        assert_eq!(decl.name(), Some("IntBox".to_string()));
        let has_conformance = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::ConformanceList);
        assert!(has_conformance, "Expected ConformanceList node");
    }

    #[test]
    fn test_struct_full_syntax() {
        let source = "struct Box[T]: Container[T] where T: Equatable { }";
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

        let has_type_params = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::TypeParameterList);
        assert!(has_type_params, "Expected TypeParameterList node");

        let has_conformance = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::ConformanceList);
        assert!(has_conformance, "Expected ConformanceList node");

        let has_where_clause = decl.syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::WhereClause);
        assert!(has_where_clause, "Expected WhereClause node");
    }
}
