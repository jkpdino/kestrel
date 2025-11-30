use std::sync::Arc;

use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::expression::ExpressionSymbol;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::Resolver;
use crate::utils::get_node_span;

/// Resolver for expression nodes
pub struct ExpressionResolver;

impl Resolver for ExpressionResolver {
    fn build_declaration(
        &self,
        syntax: &SyntaxNode,
        source: &str,
        parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        _root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        let span = get_node_span(syntax, source);

        // Find the actual expression variant (first child of Expression node)
        let expr_node = syntax.children().next()?;

        let expr_symbol = match expr_node.kind() {
            SyntaxKind::ExprUnit => {
                ExpressionSymbol::unit(span, parent.cloned())
            }
            SyntaxKind::ExprInteger => {
                let value = parse_integer(&expr_node, source)?;
                ExpressionSymbol::integer(value, span, parent.cloned())
            }
            SyntaxKind::ExprFloat => {
                let value = parse_float(&expr_node, source)?;
                ExpressionSymbol::float(value, span, parent.cloned())
            }
            SyntaxKind::ExprString => {
                let value = parse_string(&expr_node, source)?;
                ExpressionSymbol::string(value, span, parent.cloned())
            }
            SyntaxKind::ExprBool => {
                let value = parse_bool(&expr_node, source)?;
                ExpressionSymbol::bool(value, span, parent.cloned())
            }
            SyntaxKind::ExprArray => {
                // For now, create without child expressions - they'll be resolved separately
                let elements = vec![];
                ExpressionSymbol::array(elements, span, None, parent.cloned())
            }
            SyntaxKind::ExprTuple => {
                let elements = vec![];
                ExpressionSymbol::tuple(elements, span, parent.cloned())
            }
            SyntaxKind::ExprGrouping => {
                // For grouping, we'd need to resolve the inner expression first
                // For now, create a placeholder unit
                ExpressionSymbol::unit(span, parent.cloned())
            }
            _ => return None,
        };

        let expr_arc = Arc::new(expr_symbol);
        let expr_arc_dyn = expr_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&expr_arc_dyn);
        }

        Some(expr_arc)
    }

    fn is_terminal(&self) -> bool {
        // Expressions can contain nested expressions, but we handle them explicitly
        true
    }
}

/// Parse an integer literal from the syntax node
fn parse_integer(node: &SyntaxNode, _source: &str) -> Option<i64> {
    let token = node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tok| tok.kind() == SyntaxKind::Integer)?;

    let text = token.text();
    parse_integer_value(text)
}

/// Parse an integer value from a string, handling different bases
fn parse_integer_value(text: &str) -> Option<i64> {
    if text.starts_with("0x") || text.starts_with("0X") {
        i64::from_str_radix(&text[2..], 16).ok()
    } else if text.starts_with("0b") || text.starts_with("0B") {
        i64::from_str_radix(&text[2..], 2).ok()
    } else if text.starts_with("0o") || text.starts_with("0O") {
        i64::from_str_radix(&text[2..], 8).ok()
    } else {
        text.parse().ok()
    }
}

/// Parse a float literal from the syntax node
fn parse_float(node: &SyntaxNode, _source: &str) -> Option<f64> {
    let token = node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tok| tok.kind() == SyntaxKind::Float)?;

    token.text().parse().ok()
}

/// Parse a string literal from the syntax node
fn parse_string(node: &SyntaxNode, _source: &str) -> Option<String> {
    let token = node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tok| tok.kind() == SyntaxKind::String)?;

    let text = token.text();
    // Remove surrounding quotes
    if text.len() >= 2 && text.starts_with('"') && text.ends_with('"') {
        Some(unescape_string(&text[1..text.len() - 1]))
    } else {
        Some(text.to_string())
    }
}

/// Unescape a string literal (basic implementation)
fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some('x') => {
                    // Hex escape: \xNN
                    let hex: String = chars.by_ref().take(2).collect();
                    if let Ok(val) = u8::from_str_radix(&hex, 16) {
                        result.push(val as char);
                    }
                }
                Some('u') => {
                    // Unicode escape: \u{NNNN}
                    if chars.peek() == Some(&'{') {
                        chars.next();
                        let hex: String = chars.by_ref().take_while(|&c| c != '}').collect();
                        if let Ok(val) = u32::from_str_radix(&hex, 16) {
                            if let Some(c) = char::from_u32(val) {
                                result.push(c);
                            }
                        }
                    }
                }
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Parse a boolean literal from the syntax node
fn parse_bool(node: &SyntaxNode, _source: &str) -> Option<bool> {
    let token = node
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tok| tok.kind() == SyntaxKind::Boolean)?;

    match token.text() {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer_decimal() {
        assert_eq!(parse_integer_value("42"), Some(42));
        assert_eq!(parse_integer_value("0"), Some(0));
        assert_eq!(parse_integer_value("12345"), Some(12345));
    }

    #[test]
    fn test_parse_integer_hex() {
        assert_eq!(parse_integer_value("0xFF"), Some(255));
        assert_eq!(parse_integer_value("0XAB"), Some(171));
        assert_eq!(parse_integer_value("0x1a2b"), Some(6699));
    }

    #[test]
    fn test_parse_integer_binary() {
        assert_eq!(parse_integer_value("0b1010"), Some(10));
        assert_eq!(parse_integer_value("0B1111"), Some(15));
    }

    #[test]
    fn test_parse_integer_octal() {
        assert_eq!(parse_integer_value("0o17"), Some(15));
        assert_eq!(parse_integer_value("0O755"), Some(493));
    }

    #[test]
    fn test_unescape_string_basic() {
        assert_eq!(unescape_string("hello"), "hello");
        assert_eq!(unescape_string("hello\\nworld"), "hello\nworld");
        assert_eq!(unescape_string("tab\\there"), "tab\there");
        assert_eq!(unescape_string("quote\\\"here"), "quote\"here");
    }

    #[test]
    fn test_unescape_string_hex() {
        assert_eq!(unescape_string("\\x41"), "A");
        assert_eq!(unescape_string("\\x48\\x69"), "Hi");
    }

    #[test]
    fn test_unescape_string_unicode() {
        assert_eq!(unescape_string("\\u{1F600}"), "😀");
        assert_eq!(unescape_string("\\u{41}"), "A");
    }
}
