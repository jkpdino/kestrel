use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::module::{emit_module_path, ModulePath};
use crate::event::EventSink;

/// Represents an import declaration
///
/// The declaration is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDeclaration {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl ImportDeclaration {
    /// Get the module path from this import declaration
    pub fn path(&self) -> ModulePath {
        self.syntax
            .children()
            .find(|node| node.kind() == SyntaxKind::ModulePath)
            .map(|node| ModulePath { syntax: node })
            .expect("ImportDeclaration must have a ModulePath child")
    }

    /// Check if this is an "import all" declaration (e.g., `import A.B.C`)
    pub fn is_import_all(&self) -> bool {
        // If there's no "as" keyword and no items list, it's import all
        !self.has_alias() && !self.has_items()
    }

    /// Check if this import has an alias (e.g., `import A.B.C as D`)
    pub fn has_alias(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|elem| elem.as_token().map(|t| t.kind() == SyntaxKind::As).unwrap_or(false))
            && !self.has_items()
    }

    /// Check if this import has an items list (e.g., `import A.B.C.(D, E)`)
    pub fn has_items(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|elem| elem.as_token().map(|t| t.kind() == SyntaxKind::LParen).unwrap_or(false))
    }

    /// Get the alias identifier if present (for `import A.B.C as D`)
    pub fn alias(&self) -> Option<String> {
        if !self.has_alias() {
            return None;
        }

        // Find the identifier after the "as" keyword
        let mut found_as = false;
        for elem in self.syntax.children_with_tokens() {
            if let Some(token) = elem.as_token() {
                if found_as && token.kind() == SyntaxKind::Identifier {
                    return Some(token.text().to_string());
                }
                if token.kind() == SyntaxKind::As {
                    found_as = true;
                }
            }
        }
        None
    }

    /// Get the import items if present (for `import A.B.C.(D, E)`)
    pub fn items(&self) -> Vec<SyntaxNode> {
        self.syntax
            .children()
            .filter(|node| node.kind() == SyntaxKind::ImportItem)
            .collect()
    }
}

/// Internal parser for module path segments
fn module_path_parser_internal() -> impl Parser<Token, Vec<Span>, Error = Simple<Token>> + Clone {
    filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .separated_by(just(Token::Dot))
    .at_least(1)
}

/// Internal parser for an import item (identifier or identifier as alias)
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

/// Internal parser for import items list: (D, E) or (D as E, F as G)
fn import_items_parser_internal() -> impl Parser<Token, Vec<(Span, Option<Span>)>, Error = Simple<Token>> + Clone {
    just(Token::LParen)
        .ignore_then(
            import_item_parser_internal()
                .separated_by(just(Token::Comma))
                .at_least(1)
        )
        .then_ignore(just(Token::RParen))
}

/// Internal parser for an import declaration
/// Returns the raw parsed data needed to build the ImportDeclaration
fn import_declaration_parser_internal() -> impl Parser<Token, (Span, Vec<Span>, Option<Span>, Option<Vec<(Span, Option<Span>)>>), Error = Simple<Token>> + Clone {
    just(Token::Import)
        .map_with_span(|_, span| span)
        .then(module_path_parser_internal())
        .then(
            // Optional: either "as Alias" or ".(items)"
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

/// Parse an import declaration and emit events
/// This is the primary event-driven parser function
pub fn parse_import_declaration<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match import_declaration_parser_internal().parse(stream) {
        Ok((import_span, path_segments, alias, items)) => {
            emit_import_declaration(sink, import_span, &path_segments, alias, items);
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

/// Emit events for an import declaration
/// Internal helper function
fn emit_import_declaration(
    sink: &mut EventSink,
    import_span: Span,
    path_segments: &[Span],
    alias: Option<Span>,
    items: Option<Vec<(Span, Option<Span>)>>,
) {
    // Start import declaration
    sink.start_node(SyntaxKind::ImportDeclaration);
    sink.add_token(SyntaxKind::Import, import_span);

    // Add module path
    emit_module_path(sink, path_segments);

    // Emit appropriate events based on import type
    if let Some(items_list) = &items {
        // import A.B.C.(D, E) or import A.B.C.(D as E, F as G)
        let last_segment_end = path_segments.last().unwrap().end;
        sink.add_token(SyntaxKind::Dot, last_segment_end..last_segment_end + 1);
        sink.add_token(SyntaxKind::LParen, last_segment_end + 1..last_segment_end + 2);

        for (i, (name_span, alias_span)) in items_list.iter().enumerate() {
            if i > 0 {
                // Find comma between items
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

        // Find closing paren
        let last_item = items_list.last().unwrap();
        let last_item_end = if let Some(alias_s) = &last_item.1 {
            alias_s.end
        } else {
            last_item.0.end
        };
        sink.add_token(SyntaxKind::RParen, last_item_end..last_item_end + 1);
    } else if let Some(alias_span) = alias {
        // import A.B.C as D
        let as_start = path_segments.last().unwrap().end + 1;
        sink.add_token(SyntaxKind::As, as_start..as_start + 2);
        sink.add_token(SyntaxKind::Identifier, alias_span);
    }
    // else: import A.B.C (no additional tokens needed)

    sink.finish_node();
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;
    use crate::event::TreeBuilder;

    #[test]
    fn test_import_all() {
        let source = "import A.B.C";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_import_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ImportDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.path().segment_names(), vec!["A", "B", "C"]);
        assert!(decl.is_import_all());
        assert_eq!(decl.syntax.kind(), SyntaxKind::ImportDeclaration);
    }

    #[test]
    fn test_import_aliased() {
        let source = "import A.B.C as D";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_import_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ImportDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.path().segment_names(), vec!["A", "B", "C"]);
        assert!(decl.has_alias());
        assert_eq!(decl.alias(), Some("D".to_string()));
    }

    #[test]
    fn test_import_items() {
        let source = "import A.B.C.(D, E)";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_import_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ImportDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.path().segment_names(), vec!["A", "B", "C"]);
        assert!(decl.has_items());

        let items = decl.items();
        assert_eq!(items.len(), 2);

        // Check first item (D)
        let first_id = items[0].children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::Identifier)
            .unwrap();
        assert_eq!(first_id.text(), "D");

        // Check second item (E)
        let second_id = items[1].children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::Identifier)
            .unwrap();
        assert_eq!(second_id.text(), "E");
    }

    #[test]
    fn test_import_aliased_items() {
        let source = "import A.B.C.(D as E, F as G)";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_import_declaration(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        let decl = ImportDeclaration {
            syntax: tree,
            span: 0..source.len(),
        };

        assert_eq!(decl.path().segment_names(), vec!["A", "B", "C"]);
        assert!(decl.has_items());

        let items = decl.items();
        assert_eq!(items.len(), 2);

        // Check first item (D as E)
        let first_tokens: Vec<_> = items[0].children_with_tokens()
            .filter_map(|e| e.into_token())
            .filter(|t| t.kind() == SyntaxKind::Identifier)
            .collect();
        assert_eq!(first_tokens.len(), 2);
        assert_eq!(first_tokens[0].text(), "D");
        assert_eq!(first_tokens[1].text(), "E");

        // Check second item (F as G)
        let second_tokens: Vec<_> = items[1].children_with_tokens()
            .filter_map(|e| e.into_token())
            .filter(|t| t.kind() == SyntaxKind::Identifier)
            .collect();
        assert_eq!(second_tokens.len(), 2);
        assert_eq!(second_tokens[0].text(), "F");
        assert_eq!(second_tokens[1].text(), "G");
    }
}
