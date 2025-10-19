//! Common event emitters shared across multiple parsers
//!
//! This module provides reusable event emission functions that build syntax
//! trees by emitting events through an EventSink. These functions are used
//! by multiple parser modules to avoid code duplication.

use kestrel_span::Span;
use kestrel_syntax_tree::SyntaxKind;

use crate::event::EventSink;

/// Emit events for a module path
///
/// Emits a ModulePath node containing identifier tokens separated by dot tokens.
/// This function handles the dot placement between identifiers.
///
/// # Structure
/// ```text
/// ModulePath
///   Identifier("A")
///   Dot
///   Identifier("B")
///   Dot
///   Identifier("C")
/// ```
///
/// # Arguments
/// - `sink`: The event sink to emit events to
/// - `segments`: Spans for each identifier segment in the path
///
/// # Examples
/// - `segments = [span(A)]` → emits `A`
/// - `segments = [span(A), span(B), span(C)]` → emits `A.B.C`
pub fn emit_module_path(sink: &mut EventSink, segments: &[Span]) {
    sink.start_node(SyntaxKind::ModulePath);
    for (i, span) in segments.iter().enumerate() {
        if i > 0 {
            // Emit dot token between segments
            // Note: Assumes dot is immediately before the current segment
            sink.add_token(SyntaxKind::Dot, span.start - 1..span.start);
        }
        sink.add_token(SyntaxKind::Identifier, span.clone());
    }
    sink.finish_node();
}

/// Emit events for an import declaration
///
/// Emits an ImportDeclaration node with support for three forms:
/// - Import all: `import A.B.C`
/// - Import with alias: `import A.B.C as D`
/// - Import specific items: `import A.B.C.(D, E)` or `import A.B.C.(D as E, F as G)`
///
/// # Structure (Import all)
/// ```text
/// ImportDeclaration
///   Import
///   ModulePath
///     ...
/// ```
///
/// # Structure (Import with alias)
/// ```text
/// ImportDeclaration
///   Import
///   ModulePath
///     ...
///   As
///   Identifier("D")
/// ```
///
/// # Structure (Import items)
/// ```text
/// ImportDeclaration
///   Import
///   ModulePath
///     ...
///   Dot
///   LParen
///   ImportItem
///     Identifier("D")
///     [As Identifier("E")]
///   Comma
///   ImportItem
///     Identifier("F")
///   RParen
/// ```
///
/// # Arguments
/// - `sink`: The event sink to emit events to
/// - `import_span`: Span of the `import` keyword
/// - `path_segments`: Spans for each identifier in the module path
/// - `alias`: Optional span for the alias identifier (for `import A as B`)
/// - `items`: Optional list of import items with optional aliases (for `import A.(B, C)`)
pub fn emit_import_declaration(
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
