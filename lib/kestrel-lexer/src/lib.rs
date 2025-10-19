use logos::Logos;
use unicode_xid::UnicodeXID;
pub use kestrel_span::{Span, Spanned};

/// Check if a string is a valid Unicode identifier
fn is_valid_identifier(lex: &mut logos::Lexer<Token>) -> bool {
    let slice = lex.slice();
    let mut chars = slice.chars();

    // First character must be XID_Start or underscore
    if let Some(first) = chars.next() {
        if !first.is_xid_start() && first != '_' {
            return false;
        }
    } else {
        return false;
    }

    // Remaining characters must be XID_Continue
    chars.all(|c| c.is_xid_continue())
}

/// Skip nested block comments
fn skip_block_comment(lex: &mut logos::Lexer<Token>) -> logos::Skip {
    let remainder = lex.remainder();
    let mut depth = 1;
    let mut chars = remainder.chars();
    let mut offset = 0;

    while let Some(c) = chars.next() {
        offset += c.len_utf8();

        if c == '/' {
            if let Some('*') = chars.clone().next() {
                chars.next();
                offset += 1;
                depth += 1;
            }
        } else if c == '*' {
            if let Some('/') = chars.clone().next() {
                chars.next();
                offset += 1;
                depth -= 1;
                if depth == 0 {
                    lex.bump(offset);
                    return logos::Skip;
                }
            }
        }
    }

    // Unclosed comment - bump to end
    lex.bump(offset);
    logos::Skip
}

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
#[logos(skip r"[ \t\n\f]+")]  // Whitespace
#[logos(skip r"//[^\n]*")]     // Line comments
pub enum Token {
    // ===== Comments =====
    // Comments are handled by skip patterns above and below:
    // - Line comments: // to end of line  (skipped automatically)
    // - Block comments: /* */ with nesting support (handled by callback below)
    //
    // Note: BlockCommentStart is never emitted as a token - it's used internally
    // to trigger the skip_block_comment callback which handles nested comments
    #[regex(r"/\*", skip_block_comment)]
    #[doc(hidden)]
    BlockCommentStart,

    // ===== Literals =====
    // Match potential Unicode identifiers and validate with XID rules
    #[regex(r"[\p{L}_][\p{L}\p{N}_]*", is_valid_identifier)]
    Identifier,

    #[regex(r#""([^"\\]|\\.)*""#)]
    String,

    #[regex(r"[0-9]+")]
    Integer,

    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?")]
    Float,

    #[token("true")]
    #[token("false")]
    Boolean,

    #[token("null")]
    Null,

    // ===== Declaration Keywords =====
    #[token("class")]
    Class,

    #[token("fileprivate")]
    Fileprivate,

    #[token("fn")]
    Fn,

    #[token("import")]
    Import,

    #[token("internal")]
    Internal,

    #[token("let")]
    Let,

    #[token("module")]
    Module,

    #[token("private")]
    Private,

    #[token("public")]
    Public,

    // ===== Statement Keywords =====
    #[token("as")]
    As,

    #[token("else")]
    Else,

    #[token("if")]
    If,

    // ===== Braces =====
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    // ===== Punctuation =====
    #[token(";")]
    Semicolon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token(":")]
    Colon,

    #[token("!")]
    Bang,

    // ===== Operators =====
    #[token("=")]
    Equals,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("->")]
    Arrow,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,
}

pub type SpannedToken = Spanned<Token>;

/// Lex source code and return an iterator of tokens with their spans
pub fn lex(source: &str) -> impl Iterator<Item = Result<SpannedToken, Spanned<()>>> + '_ {
    Token::lexer(source).spanned().map(|(token, span)| {
        token
            .map(|t| Spanned::new(t, span.clone()))
            .map_err(|_| Spanned::new((), span))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "fn main() { let x = 42; }";
        let tokens: Vec<_> = lex(source).collect();

        assert!(tokens.len() > 0);

        // First token should be 'fn' at position 0..2
        if let Ok(spanned) = &tokens[0] {
            assert_eq!(spanned.value, Token::Fn);
            assert_eq!(spanned.span, 0..2);
        }
    }

    #[test]
    fn test_spans() {
        let source = "let x = 42";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        // Verify spans don't overlap and cover the source
        assert_eq!(tokens[0].span, 0..3);   // "let"
        assert_eq!(tokens[1].span, 4..5);   // "x"
        assert_eq!(tokens[2].span, 6..7);   // "="
        assert_eq!(tokens[3].span, 8..10);  // "42"
    }

    #[test]
    fn test_literals() {
        // Test string literals
        let source = r#""hello world""#;
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();
        assert_eq!(tokens[0].value, Token::String);

        // Test integer literals
        let source = "42";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();
        assert_eq!(tokens[0].value, Token::Integer);

        // Test float literals
        let source = "3.14 2.5e10 1.0E-5";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();
        assert_eq!(tokens[0].value, Token::Float);
        assert_eq!(tokens[1].value, Token::Float);
        assert_eq!(tokens[2].value, Token::Float);

        // Test boolean literals
        let source = "true false";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();
        assert_eq!(tokens[0].value, Token::Boolean);
        assert_eq!(tokens[1].value, Token::Boolean);

        // Test null literal
        let source = "null";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();
        assert_eq!(tokens[0].value, Token::Null);
    }

    #[test]
    fn test_module_declaration() {
        let source = "module A.B.C";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].value, Token::Module);
        assert_eq!(tokens[1].value, Token::Identifier);
        assert_eq!(tokens[2].value, Token::Dot);
        assert_eq!(tokens[3].value, Token::Identifier);
        assert_eq!(tokens[4].value, Token::Dot);
        assert_eq!(tokens[5].value, Token::Identifier);
    }

    #[test]
    fn test_unicode_identifiers() {
        // Test various Unicode identifier patterns
        let source = "let café = 42";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].value, Token::Let);
        assert_eq!(tokens[1].value, Token::Identifier); // café
        assert_eq!(tokens[2].value, Token::Equals);
        assert_eq!(tokens[3].value, Token::Integer);

        // Test Greek identifiers
        let source = "fn αβγ() { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens[0].value, Token::Fn);
        assert_eq!(tokens[1].value, Token::Identifier); // αβγ

        // Test mixed scripts
        let source = "let _hello世界 = 42";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens[1].value, Token::Identifier); // _hello世界
    }

    #[test]
    fn test_line_comments() {
        let source = r#"
            let x = 42; // This is a comment
            let y = 10; // Another comment
        "#;
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        // Comments should be skipped
        // Tokens: let x = 42 ; let y = 10 ;
        assert_eq!(tokens.len(), 10);
        assert_eq!(tokens[0].value, Token::Let);
        assert_eq!(tokens[1].value, Token::Identifier); // x
        assert_eq!(tokens[2].value, Token::Equals);
        assert_eq!(tokens[3].value, Token::Integer); // 42
        assert_eq!(tokens[4].value, Token::Semicolon);
        assert_eq!(tokens[5].value, Token::Let);
        assert_eq!(tokens[6].value, Token::Identifier); // y
        assert_eq!(tokens[7].value, Token::Equals);
        assert_eq!(tokens[8].value, Token::Integer); // 10
        assert_eq!(tokens[9].value, Token::Semicolon);
    }

    #[test]
    fn test_block_comments() {
        let source = r#"
            let x = /* comment */ 42;
            /* multi
               line
               comment */
            let y = 10;
        "#;
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        // Comments should be skipped
        // Tokens: let x = 42 ; let y = 10 ;
        assert_eq!(tokens.len(), 10);
        assert_eq!(tokens[0].value, Token::Let);
        assert_eq!(tokens[1].value, Token::Identifier); // x
        assert_eq!(tokens[2].value, Token::Equals);
        assert_eq!(tokens[3].value, Token::Integer); // 42
        assert_eq!(tokens[4].value, Token::Semicolon);
        assert_eq!(tokens[5].value, Token::Let);
        assert_eq!(tokens[6].value, Token::Identifier); // y
        assert_eq!(tokens[7].value, Token::Equals);
        assert_eq!(tokens[8].value, Token::Integer); // 10
        assert_eq!(tokens[9].value, Token::Semicolon);
    }

    #[test]
    fn test_nested_comments() {
        let source = r#"
            let x = /* outer /* inner */ still outer */ 42;
            let y = /* /* /* deeply */ nested */ comments */ 10;
        "#;
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        // All nested comments should be properly handled
        // Tokens: let x = 42 ; let y = 10 ;
        assert_eq!(tokens.len(), 10);
        assert_eq!(tokens[0].value, Token::Let);
        assert_eq!(tokens[1].value, Token::Identifier); // x
        assert_eq!(tokens[2].value, Token::Equals);
        assert_eq!(tokens[3].value, Token::Integer); // 42
        assert_eq!(tokens[4].value, Token::Semicolon);
        assert_eq!(tokens[5].value, Token::Let);
        assert_eq!(tokens[6].value, Token::Identifier); // y
        assert_eq!(tokens[7].value, Token::Equals);
        assert_eq!(tokens[8].value, Token::Integer); // 10
        assert_eq!(tokens[9].value, Token::Semicolon);
    }

    #[test]
    fn test_comments_dont_affect_strings() {
        let source = r#"let s = "// not a comment";"#;
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens.len(), 5); // let s = "..." ;
        assert_eq!(tokens[0].value, Token::Let);
        assert_eq!(tokens[1].value, Token::Identifier); // s
        assert_eq!(tokens[2].value, Token::Equals);
        assert_eq!(tokens[3].value, Token::String);
        assert_eq!(tokens[4].value, Token::Semicolon);
    }

    #[test]
    fn test_import_keyword() {
        let source = "import A.B.C";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].value, Token::Import);
        assert_eq!(tokens[1].value, Token::Identifier); // A
        assert_eq!(tokens[2].value, Token::Dot);
        assert_eq!(tokens[3].value, Token::Identifier); // B
        assert_eq!(tokens[4].value, Token::Dot);
        assert_eq!(tokens[5].value, Token::Identifier); // C
    }

    #[test]
    fn test_import_with_as() {
        let source = "import A.B.C as D";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].value, Token::Import);
        assert_eq!(tokens[1].value, Token::Identifier); // A
        assert_eq!(tokens[2].value, Token::Dot);
        assert_eq!(tokens[3].value, Token::Identifier); // B
        assert_eq!(tokens[4].value, Token::Dot);
        assert_eq!(tokens[5].value, Token::Identifier); // C
        assert_eq!(tokens[6].value, Token::As);
        assert_eq!(tokens[7].value, Token::Identifier); // D
    }

    #[test]
    fn test_import_with_list() {
        let source = "import A.B.C.(D, E)";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens.len(), 12);
        assert_eq!(tokens[0].value, Token::Import);
        assert_eq!(tokens[1].value, Token::Identifier); // A
        assert_eq!(tokens[2].value, Token::Dot);
        assert_eq!(tokens[3].value, Token::Identifier); // B
        assert_eq!(tokens[4].value, Token::Dot);
        assert_eq!(tokens[5].value, Token::Identifier); // C
        assert_eq!(tokens[6].value, Token::Dot);
        assert_eq!(tokens[7].value, Token::LParen);
        assert_eq!(tokens[8].value, Token::Identifier); // D
        assert_eq!(tokens[9].value, Token::Comma);
        assert_eq!(tokens[10].value, Token::Identifier); // E
        assert_eq!(tokens[11].value, Token::RParen);
    }

    #[test]
    fn test_class_declaration() {
        let source = "class Foo { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].value, Token::Class);
        assert_eq!(tokens[1].value, Token::Identifier); // Foo
        assert_eq!(tokens[2].value, Token::LBrace);
        assert_eq!(tokens[3].value, Token::RBrace);
    }

    #[test]
    fn test_class_with_visibility() {
        let source = "public class Foo { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].value, Token::Public);
        assert_eq!(tokens[1].value, Token::Class);
        assert_eq!(tokens[2].value, Token::Identifier); // Foo
        assert_eq!(tokens[3].value, Token::LBrace);
        assert_eq!(tokens[4].value, Token::RBrace);

        // Test other visibility modifiers
        let source = "private class Bar { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();
        assert_eq!(tokens[0].value, Token::Private);

        let source = "fileprivate class Baz { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();
        assert_eq!(tokens[0].value, Token::Fileprivate);

        let source = "internal class Qux { }";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .collect();
        assert_eq!(tokens[0].value, Token::Internal);
    }
}
