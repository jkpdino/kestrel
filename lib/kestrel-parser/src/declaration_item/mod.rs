use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::module::{ModuleDeclaration, parse_module_declaration};
use crate::import::{ImportDeclaration, parse_import_declaration};
use crate::class::{ClassDeclaration, parse_class_declaration};
use crate::event::EventSink;
use crate::common::{
    module_declaration_parser_internal,
    visibility_parser_internal, import_declaration_parser_internal,
};

/// Represents a declaration item - a top-level unit of code in a Kestrel file
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarationItem {
    Module(ModuleDeclaration),
    Import(ImportDeclaration),
    Class(ClassDeclaration),
}

impl DeclarationItem {
    /// Get the span of this declaration item
    pub fn span(&self) -> &Span {
        match self {
            DeclarationItem::Module(decl) => &decl.span,
            DeclarationItem::Import(decl) => &decl.span,
            DeclarationItem::Class(decl) => &decl.span,
        }
    }

    /// Get the syntax tree for this declaration item
    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            DeclarationItem::Module(decl) => &decl.syntax,
            DeclarationItem::Import(decl) => &decl.syntax,
            DeclarationItem::Class(decl) => &decl.syntax,
        }
    }
}

/// Raw parsed data for a declaration item
#[derive(Debug, Clone)]
enum DeclarationItemData {
    Module(Span, Vec<Span>),
    Import(Span, Vec<Span>, Option<Span>, Option<Vec<(Span, Option<Span>)>>),
    Class(Option<(Token, Span)>, Span, Span, Span, Vec<DeclarationItemData>, Span),
}

/// Internal Chumsky parser for a single declaration item
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

/// Internal Chumsky parser for multiple declaration items with error recovery
///
/// This parser attempts to parse multiple declarations. When a single declaration fails,
/// Chumsky's error recovery will collect the error and the `.repeated()` combinator will
/// continue attempting to parse subsequent declarations. This allows the parser to report
/// multiple errors in a single pass.
fn declaration_items_parser_internal() -> impl Parser<Token, Vec<DeclarationItemData>, Error = Simple<Token>> + Clone {
    declaration_item_parser_internal()
        .repeated()
        .at_least(0)
}

/// Parse a declaration item and emit events
/// This is the primary event-driven parser function
/// Tries to parse as a module declaration first, then import, then class
pub fn parse_declaration_item<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    // Clone the iterator so we can try multiple parsers
    let tokens_clone1 = tokens.clone();
    let tokens_clone2 = tokens.clone();

    // Try parsing as module declaration
    let module_result = {
        let mut temp_sink = EventSink::new();
        parse_module_declaration(source, tokens, &mut temp_sink);

        // Check if there were errors
        let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
        if !has_errors {
            // Success! Copy events to the main sink
            for event in temp_sink.into_events() {
                match event {
                    crate::event::Event::StartNode(kind) => sink.start_node(kind),
                    crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                    crate::event::Event::FinishNode => sink.finish_node(),
                    crate::event::Event::Error { message, span } => sink.error(message, span),
                }
            }
            return;
        }
        has_errors
    };

    // If module parsing failed, try import declaration
    if module_result {
        let mut temp_sink = EventSink::new();
        parse_import_declaration(source, tokens_clone1, &mut temp_sink);

        // Check if there were errors
        let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
        if !has_errors {
            // Success! Copy events to the main sink
            for event in temp_sink.into_events() {
                match event {
                    crate::event::Event::StartNode(kind) => sink.start_node(kind),
                    crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                    crate::event::Event::FinishNode => sink.finish_node(),
                    crate::event::Event::Error { message, span } => sink.error(message, span),
                }
            }
            return;
        }
    }

    // If import parsing failed, try class declaration
    let mut temp_sink = EventSink::new();
    parse_class_declaration(source, tokens_clone2, &mut temp_sink);

    // Check if there were errors
    let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
    if !has_errors {
        // Success! Copy events to the main sink
        for event in temp_sink.into_events() {
            match event {
                crate::event::Event::StartNode(kind) => sink.start_node(kind),
                crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                crate::event::Event::FinishNode => sink.finish_node(),
                crate::event::Event::Error { message, span } => sink.error(message, span),
            }
        }
        return;
    }

    // All failed - emit error (no specific span available since all parsers failed)
    sink.error_no_span("Expected module, import, or class declaration".to_string());
}

/// Parse a source file (multiple declaration items) and emit events
/// This creates a SourceFile root node containing all declarations
pub fn parse_source_file<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    sink.start_node(SyntaxKind::SourceFile);

    match declaration_items_parser_internal().parse(stream) {
        Ok(items) => {
            // Emit events for each declaration item
            for item_data in items {
                match item_data {
                    DeclarationItemData::Module(module_span, path_segments) => {
                        // Emit module declaration events
                        sink.start_node(SyntaxKind::ModuleDeclaration);
                        sink.add_token(SyntaxKind::Module, module_span);
                        crate::common::emit_module_path(sink, &path_segments);
                        sink.finish_node();
                    }
                    DeclarationItemData::Import(import_span, path_segments, alias, items) => {
                        // Emit import declaration events
                        crate::common::emit_import_declaration(sink, import_span, &path_segments, alias, items);
                    }
                    DeclarationItemData::Class(visibility, class_span, name_span, lbrace_span, body, rbrace_span) => {
                        // Emit class declaration events
                        emit_class_declaration(sink, visibility, class_span, name_span, lbrace_span, body, rbrace_span);
                    }
                }
            }
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

    sink.finish_node();
}

/// Emit events for a class declaration
/// Helper function used by parse_source_file
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
        emit_declaration_item_internal(sink, item_data);
    }

    sink.add_token(SyntaxKind::RBrace, rbrace_span);
    sink.finish_node(); // Finish ClassBody

    sink.finish_node(); // Finish ClassDeclaration
}

/// Emit events for a declaration item (internal recursive helper)
/// Helper function used by emit_class_declaration
fn emit_declaration_item_internal(sink: &mut EventSink, item_data: DeclarationItemData) {
    match item_data {
        DeclarationItemData::Module(module_span, path_segments) => {
            sink.start_node(SyntaxKind::ModuleDeclaration);
            sink.add_token(SyntaxKind::Module, module_span);
            crate::common::emit_module_path(sink, &path_segments);
            sink.finish_node();
        }
        DeclarationItemData::Import(import_span, path_segments, alias, items) => {
            crate::common::emit_import_declaration(sink, import_span, &path_segments, alias, items);
        }
        DeclarationItemData::Class(visibility, class_span, name_span, lbrace_span, body, rbrace_span) => {
            emit_class_declaration(sink, visibility, class_span, name_span, lbrace_span, body, rbrace_span);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;
    use kestrel_syntax_tree::SyntaxKind;

    #[test]
    fn test_declaration_item_module() {
        let source = "module A.B.C";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_declaration_item(source, tokens.into_iter(), &mut sink);

        // Check that we got module events
        let events = sink.events();
        let has_module = events.iter().any(|e| {
            matches!(e, crate::event::Event::StartNode(kind) if *kind == SyntaxKind::ModuleDeclaration)
        });
        assert!(has_module, "Should have parsed as module declaration");
    }

    #[test]
    fn test_declaration_item_single_module() {
        let source = "module Main";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_declaration_item(source, tokens.into_iter(), &mut sink);

        // Check that we got module events
        let events = sink.events();
        let has_module = events.iter().any(|e| {
            matches!(e, crate::event::Event::StartNode(kind) if *kind == SyntaxKind::ModuleDeclaration)
        });
        assert!(has_module, "Should have parsed as module declaration");
    }

    #[test]
    fn test_declaration_item_import() {
        let source = "import A.B.C";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_declaration_item(source, tokens.into_iter(), &mut sink);

        // Check that we got import events
        let events = sink.events();
        let has_import = events.iter().any(|e| {
            matches!(e, crate::event::Event::StartNode(kind) if *kind == SyntaxKind::ImportDeclaration)
        });
        assert!(has_import, "Should have parsed as import declaration");
    }
}
