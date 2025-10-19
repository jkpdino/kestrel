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

    /// Get child declaration items (nested classes, imports, modules)
    pub fn children(&self) -> Vec<SyntaxNode> {
        self.syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::ClassBody)
            .map(|body| {
                body.children()
                    .filter(|child| {
                        matches!(
                            child.kind(),
                            SyntaxKind::ClassDeclaration
                                | SyntaxKind::ImportDeclaration
                                | SyntaxKind::ModuleDeclaration
                        )
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

/// Raw parsed data for a declaration item inside a class body
#[derive(Debug, Clone)]
enum DeclarationItemData {
    Module(Span, Vec<Span>),
    Import(Span, Vec<Span>, Option<Span>, Option<Vec<(Span, Option<Span>)>>),
    Class(Option<(Token, Span)>, Span, Span, Span, Vec<DeclarationItemData>, Span),
}

/// Internal Chumsky parser for module path segments
fn module_path_parser_internal() -> impl Parser<Token, Vec<Span>, Error = Simple<Token>> + Clone {
    filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .separated_by(just(Token::Dot))
    .at_least(1)
}

/// Internal Chumsky parser for module declaration
fn module_declaration_parser_internal() -> impl Parser<Token, (Span, Vec<Span>), Error = Simple<Token>> + Clone {
    just(Token::Module)
        .map_with_span(|_, span| span)
        .then(module_path_parser_internal())
}

/// Internal parser for import item (identifier or identifier as alias)
fn import_item_parser_internal() -> impl Parser<Token, (Span, Option<Span>), Error = Simple<Token>> + Clone {
    filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .then(
        just(Token::As)
            .ignore_then(filter_map(|span, token| match token {
                Token::Identifier => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            }))
            .or_not()
    )
}

/// Internal parser for import items list
fn import_items_parser_internal() -> impl Parser<Token, Vec<(Span, Option<Span>)>, Error = Simple<Token>> + Clone {
    just(Token::LParen)
        .ignore_then(
            import_item_parser_internal()
                .separated_by(just(Token::Comma))
                .at_least(1)
        )
        .then_ignore(just(Token::RParen))
}

/// Internal parser for import declaration
fn import_declaration_parser_internal() -> impl Parser<Token, (Span, Vec<Span>, Option<Span>, Option<Vec<(Span, Option<Span>)>>), Error = Simple<Token>> + Clone {
    just(Token::Import)
        .map_with_span(|_, span| span)
        .then(module_path_parser_internal())
        .then(
            just(Token::As)
                .ignore_then(filter_map(|span, token| match token {
                    Token::Identifier => Ok(span),
                    _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
                }))
                .map(|alias| (Some(alias), None))
                .or(
                    just(Token::Dot)
                        .ignore_then(import_items_parser_internal())
                        .map(|items| (None, Some(items)))
                )
                .or_not()
        )
        .map(|((import_span, path_segments), alias_or_items)| {
            let (alias, items) = match alias_or_items {
                Some((alias, items)) => (alias, items),
                None => (None, None),
            };
            (import_span, path_segments, alias, items)
        })
}

/// Internal parser for a single declaration item
fn declaration_item_parser_internal() -> impl Parser<Token, DeclarationItemData, Error = Simple<Token>> + Clone {
    recursive(|declaration_item| {
        let module_parser = module_declaration_parser_internal()
            .map(|(span, path)| DeclarationItemData::Module(span, path));

        let import_parser = import_declaration_parser_internal()
            .map(|(import_span, path, alias, items)| DeclarationItemData::Import(import_span, path, alias, items));

        let class_parser = visibility_parser_internal()
            .then(just(Token::Class).map_with_span(|_, span| span))
            .then(filter_map(|span, token| match token {
                Token::Identifier => Ok(span),
                _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
            }))
            .then(just(Token::LBrace).map_with_span(|_, span| span))
            .then(declaration_item.repeated())
            .then(just(Token::RBrace).map_with_span(|_, span| span))
            .map(|(((((visibility, class_span), name_span), lbrace_span), body), rbrace_span)| {
                DeclarationItemData::Class(visibility, class_span, name_span, lbrace_span, body, rbrace_span)
            });

        module_parser.or(import_parser).or(class_parser)
    })
}

/// Internal Chumsky parser for class declaration
/// Returns: (visibility, class_span, name_span, lbrace_span, body, rbrace_span)
fn class_declaration_parser_internal() -> impl Parser<
    Token,
    (Option<(Token, Span)>, Span, Span, Span, Vec<DeclarationItemData>, Span),
    Error = Simple<Token>,
> + Clone {
    visibility_parser_internal()
        .then(just(Token::Class).map_with_span(|_, span| span))
        .then(filter_map(|span, token| match token {
            Token::Identifier => Ok(span),
            _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
        }))
        .then(just(Token::LBrace).map_with_span(|_, span| span))
        .then(declaration_item_parser_internal().repeated())
        .then(just(Token::RBrace).map_with_span(|_, span| span))
        .map(
            |(((((visibility, class_span), name_span), lbrace_span), body), rbrace_span)| {
                (visibility, class_span, name_span, lbrace_span, body, rbrace_span)
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
        Ok((visibility, class_span, name_span, lbrace_span, body, rbrace_span)) => {
            emit_class_declaration(
                sink,
                visibility,
                class_span,
                name_span,
                lbrace_span,
                body,
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
    body: Vec<DeclarationItemData>,
    rbrace_span: Span,
) {
    sink.start_node(SyntaxKind::ClassDeclaration);

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

    sink.add_token(SyntaxKind::Class, class_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    // Emit ClassBody node wrapping the body content
    sink.start_node(SyntaxKind::ClassBody);
    sink.add_token(SyntaxKind::LBrace, lbrace_span);

    // Emit nested declaration items
    for item_data in body {
        emit_declaration_item(sink, item_data);
    }

    sink.add_token(SyntaxKind::RBrace, rbrace_span);
    sink.finish_node(); // Finish ClassBody

    sink.finish_node(); // Finish ClassDeclaration
}

/// Emit events for a declaration item
/// Helper function used by emit_class_declaration
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
        DeclarationItemData::Class(visibility, class_span, name_span, lbrace_span, body, rbrace_span) => {
            emit_class_declaration(sink, visibility, class_span, name_span, lbrace_span, body, rbrace_span);
        }
    }
}

/// Emit events for a module path
/// Helper function
fn emit_module_path(sink: &mut EventSink, path_segments: &[Span]) {
    sink.start_node(SyntaxKind::ModulePath);
    for (i, segment_span) in path_segments.iter().enumerate() {
        if i > 0 {
            let prev_end = path_segments[i - 1].end;
            sink.add_token(SyntaxKind::Dot, prev_end..prev_end + 1);
        }
        sink.add_token(SyntaxKind::Identifier, segment_span.clone());
    }
    sink.finish_node();
}

/// Emit events for an import declaration
/// Helper function
fn emit_import_declaration(
    sink: &mut EventSink,
    import_span: Span,
    path_segments: &[Span],
    alias: Option<Span>,
    items: Option<Vec<(Span, Option<Span>)>>,
) {
    sink.start_node(SyntaxKind::ImportDeclaration);
    sink.add_token(SyntaxKind::Import, import_span);
    emit_module_path(sink, path_segments);

    if let Some(items_list) = &items {
        let last_segment_end = path_segments.last().unwrap().end;
        sink.add_token(SyntaxKind::Dot, last_segment_end..last_segment_end + 1);
        sink.add_token(SyntaxKind::LParen, last_segment_end + 1..last_segment_end + 2);

        for (i, (name_span, alias_span)) in items_list.iter().enumerate() {
            if i > 0 {
                let prev_end = if let Some(alias_s) = items_list.get(i - 1).and_then(|(_, alias)| alias.as_ref()) {
                    alias_s.end
                } else {
                    items_list.get(i - 1).unwrap().0.end
                };
                sink.add_token(SyntaxKind::Comma, prev_end..prev_end + 1);
            }

            sink.start_node(SyntaxKind::ImportItem);
            sink.add_token(SyntaxKind::Identifier, name_span.clone());

            if let Some(alias_s) = alias_span {
                let as_start = name_span.end + 1;
                sink.add_token(SyntaxKind::As, as_start..as_start + 2);
                sink.add_token(SyntaxKind::Identifier, alias_s.clone());
            }
            sink.finish_node();
        }

        let last_item = items_list.last().unwrap();
        let last_item_end = if let Some(alias_s) = &last_item.1 {
            alias_s.end
        } else {
            last_item.0.end
        };
        sink.add_token(SyntaxKind::RParen, last_item_end..last_item_end + 1);
    } else if let Some(alias_span) = alias {
        let as_start = path_segments.last().unwrap().end + 1;
        sink.add_token(SyntaxKind::As, as_start..as_start + 2);
        sink.add_token(SyntaxKind::Identifier, alias_span);
    }

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

    #[test]
    fn test_class_declaration_with_nested_class() {
        let source = "class Outer { class Inner { } }";
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

        assert_eq!(decl.name(), Some("Outer".to_string()));
        assert_eq!(decl.visibility(), None);

        // Check that there is one nested class
        let children = decl.children();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].kind(), SyntaxKind::ClassDeclaration);

        // Check the nested class name using the Name node
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
    fn test_class_declaration_with_multiple_nested_classes() {
        let source = "class Container { class First { } class Second { } }";
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

        assert_eq!(decl.name(), Some("Container".to_string()));

        // Check that there are two nested classes
        let children = decl.children();
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].kind(), SyntaxKind::ClassDeclaration);
        assert_eq!(children[1].kind(), SyntaxKind::ClassDeclaration);
    }

    #[test]
    fn test_class_declaration_deeply_nested() {
        let source = "class A { class B { class C { } } }";
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

        assert_eq!(decl.name(), Some("A".to_string()));

        // Check nested structure
        let children = decl.children();
        assert_eq!(children.len(), 1);

        // Check B has one child (C) - need to look inside ClassBody
        let b_class_body = children[0]
            .children()
            .find(|n| n.kind() == SyntaxKind::ClassBody)
            .expect("ClassBody should exist");

        let b_children: Vec<_> = b_class_body
            .children()
            .filter(|child| child.kind() == SyntaxKind::ClassDeclaration)
            .collect();
        assert_eq!(b_children.len(), 1);
    }
}
