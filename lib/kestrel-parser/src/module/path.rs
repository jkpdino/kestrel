use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode, SyntaxToken};

use crate::event::{EventSink, TreeBuilder};

/// Represents a module path like A.B.C
///
/// The module path is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModulePath {
    pub syntax: SyntaxNode,
}

impl ModulePath {
    /// Create a new ModulePath from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax }
    }

    /// Create a new ModulePath from segments, building the syntax tree
    /// This is a convenience function that emits events and builds the tree
    pub fn new(source: &str, segments: Vec<Span>) -> Self {
        let mut sink = EventSink::new();
        emit_module_path(&mut sink, &segments);
        Self::from_events(source, sink.into_events())
    }

    /// Get all identifier tokens in the path
    pub fn identifier_tokens(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .filter(|tok| tok.kind() == SyntaxKind::Identifier)
    }

    /// Extract segment names from the syntax tree
    /// Returns owned strings since the text is borrowed from the syntax tree
    pub fn segment_names(&self) -> Vec<String> {
        self.identifier_tokens()
            .map(|tok| tok.text().to_string())
            .collect()
    }

    /// Get the number of segments in the path
    pub fn segment_count(&self) -> usize {
        self.identifier_tokens().count()
    }
}

/// Internal Chumsky parser for module path
/// Returns the spans of identifier segments
fn module_path_parser_internal() -> impl Parser<Token, Vec<Span>, Error = Simple<Token>> + Clone {
    filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .separated_by(just(Token::Dot))
    .at_least(1)
}

/// Parse a module path and emit events
/// This is the primary event-driven parser function
pub fn parse_module_path<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match module_path_parser_internal().parse(stream) {
        Ok(segments) => {
            emit_module_path(sink, &segments);
        }
        Err(errors) => {
            // Emit error events for each parse error
            for error in errors {
                sink.error(format!("Parse error: {:?}", error));
            }
        }
    }
}

/// Emit events for a module path
/// Internal helper function
pub(crate) fn emit_module_path(sink: &mut EventSink, segments: &[Span]) {
    sink.start_node(SyntaxKind::ModulePath);
    for (i, span) in segments.iter().enumerate() {
        if i > 0 {
            sink.add_token(SyntaxKind::Dot, span.start - 1..span.start);
        }
        sink.add_token(SyntaxKind::Identifier, span.clone());
    }
    sink.finish_node();
}
