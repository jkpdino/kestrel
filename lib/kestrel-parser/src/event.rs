//! Event-based parsing infrastructure
//!
//! This module provides the event-based parsing architecture inspired by rust-analyzer.
//! Instead of directly building syntax trees during parsing, parsers emit events that
//! are later converted into syntax trees by a TreeBuilder.
//!
//! # Architecture
//!
//! 1. Parser emits events (StartNode, AddToken, FinishNode) to an EventSink
//! 2. EventSink collects events in a Vec
//! 3. TreeBuilder consumes events and source text to build the final syntax tree
//!
//! # Benefits
//!
//! - Decouples parsing logic from tree building
//! - Easier to implement error recovery
//! - More testable (can inspect events)
//! - Follows proven rust-analyzer architecture

use kestrel_span::Span;
use kestrel_syntax_tree::{GreenNodeBuilder, SyntaxKind, SyntaxNode};

/// Events emitted during parsing
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Event {
    /// Start a new syntax node
    StartNode(SyntaxKind),
    /// Add a token to the current node
    AddToken(SyntaxKind, Span),
    /// Finish the current syntax node
    FinishNode,
    /// A parse error occurred
    Error(String),
}

/// Collects events during parsing
#[derive(Debug, Clone)]
pub struct EventSink {
    events: Vec<Event>,
}

impl EventSink {
    /// Create a new empty event sink
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
        }
    }

    /// Start a new syntax node
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::StartNode(kind));
    }

    /// Add a token to the current node
    pub fn add_token(&mut self, kind: SyntaxKind, span: Span) {
        self.events.push(Event::AddToken(kind, span));
    }

    /// Finish the current syntax node
    pub fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    /// Record a parse error
    pub fn error(&mut self, message: String) {
        self.events.push(Event::Error(message));
    }

    /// Get the collected events
    pub fn events(&self) -> &[Event] {
        &self.events
    }

    /// Consume the sink and return the events
    pub fn into_events(self) -> Vec<Event> {
        self.events
    }
}

impl Default for EventSink {
    fn default() -> Self {
        Self::new()
    }
}

/// Builds a syntax tree from events and source text
pub struct TreeBuilder<'src> {
    source: &'src str,
    events: Vec<Event>,
    pos: usize,
}

impl<'src> TreeBuilder<'src> {
    /// Create a new tree builder
    pub fn new(source: &'src str, events: Vec<Event>) -> Self {
        Self {
            source,
            events,
            pos: 0,
        }
    }

    /// Build the syntax tree from events
    pub fn build(mut self) -> SyntaxNode {
        let mut builder = GreenNodeBuilder::new();
        self.process_events(&mut builder);
        let green = builder.finish();
        SyntaxNode::new_root(green)
    }

    /// Process all events and build the tree
    fn process_events(&mut self, builder: &mut GreenNodeBuilder) {
        while self.pos < self.events.len() {
            match &self.events[self.pos] {
                Event::StartNode(kind) => {
                    builder.start_node((*kind).into());
                    self.pos += 1;
                }
                Event::AddToken(kind, span) => {
                    let text = &self.source[span.clone()];
                    builder.token((*kind).into(), text);
                    self.pos += 1;
                }
                Event::FinishNode => {
                    builder.finish_node();
                    self.pos += 1;
                }
                Event::Error(_message) => {
                    // Skip error events when building the tree
                    // Errors can be extracted from the event list separately
                    self.pos += 1;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_sink() {
        let mut sink = EventSink::new();
        sink.start_node(SyntaxKind::ModulePath);
        sink.add_token(SyntaxKind::Identifier, 0..1);
        sink.finish_node();

        let events = sink.events();
        assert_eq!(events.len(), 3);
        assert_eq!(events[0], Event::StartNode(SyntaxKind::ModulePath));
        assert_eq!(events[1], Event::AddToken(SyntaxKind::Identifier, 0..1));
        assert_eq!(events[2], Event::FinishNode);
    }

    #[test]
    fn test_tree_builder_simple() {
        let source = "A";
        let mut sink = EventSink::new();

        sink.start_node(SyntaxKind::ModulePath);
        sink.add_token(SyntaxKind::Identifier, 0..1);
        sink.finish_node();

        let builder = TreeBuilder::new(source, sink.into_events());
        let tree = builder.build();

        assert_eq!(tree.kind(), SyntaxKind::ModulePath);
        assert_eq!(tree.children_with_tokens().count(), 1);
    }

    #[test]
    fn test_tree_builder_nested() {
        let source = "A.B";
        let mut sink = EventSink::new();

        sink.start_node(SyntaxKind::ModulePath);
        sink.add_token(SyntaxKind::Identifier, 0..1);
        sink.add_token(SyntaxKind::Dot, 1..2);
        sink.add_token(SyntaxKind::Identifier, 2..3);
        sink.finish_node();

        let builder = TreeBuilder::new(source, sink.into_events());
        let tree = builder.build();

        assert_eq!(tree.kind(), SyntaxKind::ModulePath);
        assert_eq!(tree.children_with_tokens().count(), 3);
    }

    #[test]
    fn test_tree_builder_with_child_nodes() {
        let source = "module A";
        let mut sink = EventSink::new();

        // ModuleDeclaration
        sink.start_node(SyntaxKind::ModuleDeclaration);
        sink.add_token(SyntaxKind::Module, 0..6);

        // ModulePath (child node)
        sink.start_node(SyntaxKind::ModulePath);
        sink.add_token(SyntaxKind::Identifier, 7..8);
        sink.finish_node();

        sink.finish_node();

        let builder = TreeBuilder::new(source, sink.into_events());
        let tree = builder.build();

        assert_eq!(tree.kind(), SyntaxKind::ModuleDeclaration);
        assert_eq!(tree.children().count(), 1);

        let path_node = tree.children().next().unwrap();
        assert_eq!(path_node.kind(), SyntaxKind::ModulePath);
    }
}
