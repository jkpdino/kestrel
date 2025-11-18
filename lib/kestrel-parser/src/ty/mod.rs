use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::event::{EventSink, TreeBuilder};

/// Represents a type expression
///
/// The type is stored as a lossless syntax tree. All data is derived
/// from the tree rather than stored separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyExpression {
    pub syntax: SyntaxNode,
    pub span: Span,
}

impl TyExpression {
    /// Create a new TyExpression from events and source text
    pub fn from_events(source: &str, events: Vec<crate::event::Event>, span: Span) -> Self {
        let builder = TreeBuilder::new(source, events);
        let syntax = builder.build();
        Self { syntax, span }
    }

    /// Get the kind of this type expression
    pub fn kind(&self) -> SyntaxKind {
        // Find the first child node which represents the actual type variant
        self.syntax
            .children()
            .next()
            .map(|child| child.kind())
            .unwrap_or(SyntaxKind::Error)
    }

    /// Check if this is a unit type
    pub fn is_unit(&self) -> bool {
        self.kind() == SyntaxKind::TyUnit
    }

    /// Check if this is a never type
    pub fn is_never(&self) -> bool {
        self.kind() == SyntaxKind::TyNever
    }

    /// Check if this is a tuple type
    pub fn is_tuple(&self) -> bool {
        self.kind() == SyntaxKind::TyTuple
    }

    /// Check if this is a function type
    pub fn is_function(&self) -> bool {
        self.kind() == SyntaxKind::TyFunction
    }

    /// Check if this is a path type
    pub fn is_path(&self) -> bool {
        self.kind() == SyntaxKind::TyPath
    }

    /// Get the path segments if this is a path type
    /// Structure: Ty -> TyPath -> Path -> PathElement -> Identifier
    pub fn path_segments(&self) -> Option<Vec<String>> {
        if !self.is_path() {
            return None;
        }

        // Navigate: Ty -> TyPath -> Path
        let ty_path_node = self.syntax.children().next()?;
        let path_node = ty_path_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Path)?;

        // Collect identifiers from PathElement nodes
        Some(
            path_node
                .children()
                .filter(|child| child.kind() == SyntaxKind::PathElement)
                .filter_map(|path_elem| {
                    path_elem
                        .children_with_tokens()
                        .filter_map(|elem| elem.into_token())
                        .find(|tok| tok.kind() == SyntaxKind::Identifier)
                        .map(|tok| tok.text().to_string())
                })
                .collect()
        )
    }

    /// Get the tuple element types if this is a tuple type
    /// Returns the number of elements (we don't recursively parse nested types yet)
    pub fn tuple_element_count(&self) -> Option<usize> {
        if !self.is_tuple() {
            return None;
        }

        let tuple_node = self.syntax.children().next()?;

        // Count the number of Ty child nodes
        Some(
            tuple_node
                .children()
                .filter(|child| child.kind() == SyntaxKind::Ty)
                .count()
        )
    }
}

/// Internal parser for unit type: ()
/// Note: This is now handled by parse_tuple_or_function_type_parser, but kept for reference
#[allow(dead_code)]
fn unit_type_parser() -> impl Parser<Token, (Span, Span), Error = Simple<Token>> + Clone {
    just(Token::LParen)
        .map_with_span(|_, span| span)
        .then(just(Token::RParen).map_with_span(|_, span| span))
}

/// Internal parser for never type: !
fn never_type_parser() -> impl Parser<Token, Span, Error = Simple<Token>> + Clone {
    just(Token::Bang).map_with_span(|_, span| span)
}

/// Internal parser for path type: Ident or Ident.Ident.Ident
fn path_type_parser() -> impl Parser<Token, Vec<Span>, Error = Simple<Token>> + Clone {
    filter_map(|span, token| match token {
        Token::Identifier => Ok(span),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .separated_by(just(Token::Dot))
    .at_least(1)
}

/// Combined type parser that returns a variant
pub(crate) fn ty_parser() -> impl Parser<Token, TyVariant, Error = Simple<Token>> + Clone {
    // Never type: !
    let never = never_type_parser().map(TyVariant::Never);

    // Unit type or tuple/function type
    let paren_types = parse_tuple_or_function_type_parser();

    // Path type: Ident.Ident.Ident
    let path = path_type_parser().map(TyVariant::Path);

    // Try never first, then paren types, then path
    never.or(paren_types).or(path)
}

/// Parse a type expression and emit events
/// This is the primary event-driven parser function
pub fn parse_ty<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    match ty_parser().parse(stream) {
        Ok(variant) => {
            match variant {
                TyVariant::Unit(lparen_span, rparen_span) => {
                    emit_unit_type(sink, lparen_span, rparen_span);
                }
                TyVariant::Never(bang_span) => {
                    emit_never_type(sink, bang_span);
                }
                TyVariant::Tuple(lparen, types, rparen) => {
                    emit_tuple_type(sink, lparen, types, rparen);
                }
                TyVariant::Function(lparen, params, rparen, arrow, return_ty) => {
                    emit_function_type(sink, lparen, params, rparen, arrow, return_ty);
                }
                TyVariant::Path(segments) => {
                    emit_path_type(sink, &segments);
                }
            }
        }
        Err(errors) => {
            for error in errors {
                // Chumsky errors have span information
                let span = error.span();
                sink.error_at(format!("Parse error: {:?}", error), span);
            }
        }
    }
}

/// Internal enum to distinguish between type variants during parsing
pub(crate) enum TyVariant {
    Unit(Span, Span),
    Never(Span),
    Tuple(Span, Vec<Vec<Span>>, Span),
    Function(Span, Vec<Vec<Span>>, Span, Span, Vec<Span>),
    Path(Vec<Span>),
}

/// Parser for tuple or function type (they both start with '(')
/// Tuple: (type1, type2, ...)
/// Function: (param1, param2, ...) -> return_type
/// Unit: ()
fn parse_tuple_or_function_type_parser() -> impl Parser<Token, TyVariant, Error = Simple<Token>> + Clone {
    // Parse: ( type_list ) (-> type)?
    just(Token::LParen)
        .map_with_span(|_, span| span)
        .then(
            // Parse a list of path types separated by commas
            path_type_parser()
                .separated_by(just(Token::Comma))
                .allow_trailing()
        )
        .then(just(Token::RParen).map_with_span(|_, span| span))
        .then(
            // Optional arrow and return type
            just(Token::Arrow)
                .map_with_span(|_, span| span)
                .then(path_type_parser())
                .or_not()
        )
        .map(|(((lparen, types), rparen), arrow_and_return)| {
            if let Some((arrow_span, return_ty_segments)) = arrow_and_return {
                TyVariant::Function(lparen, types, rparen, arrow_span, return_ty_segments)
            } else if types.is_empty() {
                // Empty parens = unit type
                TyVariant::Unit(lparen, rparen)
            } else {
                TyVariant::Tuple(lparen, types, rparen)
            }
        })
}

/// Emit events for a unit type
pub(crate) fn emit_unit_type(sink: &mut EventSink, lparen_span: Span, rparen_span: Span) {
    sink.start_node(SyntaxKind::Ty);
    sink.start_node(SyntaxKind::TyUnit);
    sink.add_token(SyntaxKind::LParen, lparen_span);
    sink.add_token(SyntaxKind::RParen, rparen_span);
    sink.finish_node(); // Finish TyUnit
    sink.finish_node(); // Finish Ty
}

/// Emit events for a never type
pub(crate) fn emit_never_type(sink: &mut EventSink, bang_span: Span) {
    sink.start_node(SyntaxKind::Ty);
    sink.start_node(SyntaxKind::TyNever);
    sink.add_token(SyntaxKind::Bang, bang_span);
    sink.finish_node(); // Finish TyNever
    sink.finish_node(); // Finish Ty
}

/// Helper function to emit a path structure: Path -> PathElement -> Identifier
/// This is used within TyPath nodes
fn emit_path(sink: &mut EventSink, segments: &[Span]) {
    sink.start_node(SyntaxKind::Path);

    for (i, span) in segments.iter().enumerate() {
        if i > 0 {
            // Add the dot separator (between path elements)
            sink.add_token(SyntaxKind::Dot, span.start - 1..span.start);
        }

        // Wrap each identifier in a PathElement node
        sink.start_node(SyntaxKind::PathElement);
        sink.add_token(SyntaxKind::Identifier, span.clone());
        sink.finish_node(); // Finish PathElement
    }

    sink.finish_node(); // Finish Path
}

/// Emit events for a path type
/// Structure: Ty -> TyPath -> Path -> PathElement -> Identifier
pub(crate) fn emit_path_type(sink: &mut EventSink, segments: &[Span]) {
    sink.start_node(SyntaxKind::Ty);
    sink.start_node(SyntaxKind::TyPath);
    emit_path(sink, segments);
    sink.finish_node(); // Finish TyPath
    sink.finish_node(); // Finish Ty
}

/// Emit events for a tuple type
pub(crate) fn emit_tuple_type(sink: &mut EventSink, lparen: Span, types: Vec<Vec<Span>>, rparen: Span) {
    sink.start_node(SyntaxKind::Ty);
    sink.start_node(SyntaxKind::TyTuple);

    sink.add_token(SyntaxKind::LParen, lparen);

    // Emit each type in the tuple
    for type_segments in types.iter() {
        // Each element is a nested Ty node with path structure
        sink.start_node(SyntaxKind::Ty);
        sink.start_node(SyntaxKind::TyPath);
        emit_path(sink, type_segments);
        sink.finish_node(); // Finish TyPath
        sink.finish_node(); // Finish Ty
    }

    sink.add_token(SyntaxKind::RParen, rparen);

    sink.finish_node(); // Finish TyTuple
    sink.finish_node(); // Finish Ty
}

/// Emit events for a function type
pub(crate) fn emit_function_type(
    sink: &mut EventSink,
    lparen: Span,
    params: Vec<Vec<Span>>,
    rparen: Span,
    arrow: Span,
    return_ty: Vec<Span>,
) {
    sink.start_node(SyntaxKind::Ty);
    sink.start_node(SyntaxKind::TyFunction);

    // Parameter list
    sink.start_node(SyntaxKind::TyList);
    sink.add_token(SyntaxKind::LParen, lparen);

    for param_segments in params.iter() {
        sink.start_node(SyntaxKind::Ty);
        sink.start_node(SyntaxKind::TyPath);
        emit_path(sink, param_segments);
        sink.finish_node(); // Finish TyPath
        sink.finish_node(); // Finish Ty
    }

    sink.add_token(SyntaxKind::RParen, rparen);
    sink.finish_node(); // Finish TyList

    // Arrow
    sink.add_token(SyntaxKind::Arrow, arrow);

    // Return type
    sink.start_node(SyntaxKind::Ty);
    sink.start_node(SyntaxKind::TyPath);
    emit_path(sink, &return_ty);
    sink.finish_node(); // Finish TyPath
    sink.finish_node(); // Finish Ty

    sink.finish_node(); // Finish TyFunction
    sink.finish_node(); // Finish Ty
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;

    fn parse_ty_from_source(source: &str) -> TyExpression {
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect();

        let mut sink = EventSink::new();
        parse_ty(source, tokens.into_iter(), &mut sink);

        let tree = TreeBuilder::new(source, sink.into_events()).build();
        TyExpression {
            syntax: tree,
            span: 0..source.len(),
        }
    }

    #[test]
    fn test_unit_type() {
        let source = "()";
        let ty = parse_ty_from_source(source);

        assert!(ty.is_unit());
        assert!(!ty.is_never());
        assert!(!ty.is_tuple());
        assert!(!ty.is_function());
    }

    #[test]
    fn test_never_type() {
        let source = "!";
        let ty = parse_ty_from_source(source);

        assert!(!ty.is_unit());
        assert!(ty.is_never());
        assert!(!ty.is_tuple());
        assert!(!ty.is_function());
    }

    #[test]
    fn test_path_type_simple() {
        let source = "Int";
        let ty = parse_ty_from_source(source);

        assert!(ty.is_path());
        assert_eq!(ty.path_segments(), Some(vec!["Int".to_string()]));
    }

    #[test]
    fn test_path_type_qualified() {
        let source = "A.B.C";
        let ty = parse_ty_from_source(source);

        assert!(ty.is_path());
        assert_eq!(
            ty.path_segments(),
            Some(vec!["A".to_string(), "B".to_string(), "C".to_string()])
        );
    }

    #[test]
    fn test_tuple_type_simple() {
        let source = "(Int, String)";
        let ty = parse_ty_from_source(source);

        assert!(ty.is_tuple());
        assert_eq!(ty.tuple_element_count(), Some(2));
    }

    #[test]
    fn test_tuple_type_single() {
        let source = "(Int,)";
        let ty = parse_ty_from_source(source);

        assert!(ty.is_tuple());
        assert_eq!(ty.tuple_element_count(), Some(1));
    }

    #[test]
    fn test_function_type_simple() {
        let source = "() -> Int";
        let ty = parse_ty_from_source(source);

        assert!(ty.is_function());
    }

    #[test]
    fn test_function_type_with_params() {
        let source = "(Int, String) -> Bool";
        let ty = parse_ty_from_source(source);

        assert!(ty.is_function());
    }

    #[test]
    fn test_function_type_qualified() {
        let source = "(A.B, C.D) -> E.F";
        let ty = parse_ty_from_source(source);

        assert!(ty.is_function());
    }
}
