//! Kestrel Syntax Tree
//!
//! This crate defines the syntax tree representation for the Kestrel language
//! using the `rowan` library for a lossless, resilient syntax tree implementation.
//!
//! # Overview
//!
//! The syntax tree uses `rowan`, which provides:
//! - **Lossless**: Preserves all source text including whitespace and comments
//! - **Immutable**: Syntax trees are immutable and can be safely shared
//! - **Incremental**: Supports efficient incremental parsing
//!
//! # Example
//!
//! ```
//! use kestrel_syntax_tree::{GreenNodeBuilder, SyntaxKind, SyntaxNode};
//!
//! let mut builder = GreenNodeBuilder::new();
//! builder.start_node(SyntaxKind::ModulePath.into());
//! builder.token(SyntaxKind::Identifier.into(), "Main");
//! builder.finish_node();
//!
//! let green = builder.finish();
//! let syntax = SyntaxNode::new_root(green);
//!
//! assert_eq!(syntax.kind(), SyntaxKind::ModulePath);
//! ```

use rowan::Language;
use kestrel_lexer::Token;

// Re-export for use by parsers
pub use rowan::GreenNodeBuilder;

// Define your language for rowan
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxKind {
    // ===== Syntax Nodes (Non-terminals) =====
    Root,
    SourceFile,
    DeclarationItem,
    ProtocolDeclaration,
    ProtocolBody,
    StructDeclaration,
    StructBody,
    ImportDeclaration,
    ImportItem,
    ModuleDeclaration,
    ModulePath,
    Name,
    TypeAliasDeclaration,
    AliasedType,
    FieldDeclaration,
    FunctionDeclaration,
    FunctionBody,
    ParameterList,
    Parameter,
    ReturnType,
    Visibility,
    StaticModifier,

    // Type nodes
    Ty,
    TyUnit,
    TyNever,
    TyTuple,
    TyFunction,
    TyPath,
    TyList,

    // Path nodes (shared between types and other constructs)
    Path,
    PathElement,

    // ===== Tokens (Terminals) =====
    // Literals
    Identifier,
    String,
    Integer,
    Float,
    Boolean,
    Null,

    // Keywords
    As,
    Else,
    Fileprivate,
    Func,
    If,
    Import,
    Internal,
    Let,
    Module,
    Private,
    Protocol,
    Public,
    Static,
    Struct,
    Type,
    Var,

    // Braces
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Punctuation
    Semicolon,
    Comma,
    Dot,
    Colon,
    Bang,

    // Operators
    Equals,
    Plus,
    Minus,
    Arrow,
    Star,
    Slash,

    // Trivia (whitespace and comments)
    Whitespace,
    LineComment,
    BlockComment,

    // Special
    Error,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

impl From<Token> for SyntaxKind {
    fn from(token: Token) -> Self {
        match token {
            // Trivia
            Token::Whitespace => SyntaxKind::Whitespace,
            Token::LineComment => SyntaxKind::LineComment,
            Token::BlockComment => SyntaxKind::BlockComment,
            // Literals
            Token::Identifier => SyntaxKind::Identifier,
            Token::String => SyntaxKind::String,
            Token::Integer => SyntaxKind::Integer,
            Token::Float => SyntaxKind::Float,
            Token::Boolean => SyntaxKind::Boolean,
            Token::Null => SyntaxKind::Null,
            // Keywords
            Token::As => SyntaxKind::As,
            Token::Else => SyntaxKind::Else,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            Token::Func => SyntaxKind::Func,
            Token::If => SyntaxKind::If,
            Token::Import => SyntaxKind::Import,
            Token::Internal => SyntaxKind::Internal,
            Token::Let => SyntaxKind::Let,
            Token::Module => SyntaxKind::Module,
            Token::Private => SyntaxKind::Private,
            Token::Protocol => SyntaxKind::Protocol,
            Token::Public => SyntaxKind::Public,
            Token::Static => SyntaxKind::Static,
            Token::Struct => SyntaxKind::Struct,
            Token::Type => SyntaxKind::Type,
            Token::Var => SyntaxKind::Var,
            // Braces
            Token::LParen => SyntaxKind::LParen,
            Token::RParen => SyntaxKind::RParen,
            Token::LBrace => SyntaxKind::LBrace,
            Token::RBrace => SyntaxKind::RBrace,
            Token::LBracket => SyntaxKind::LBracket,
            Token::RBracket => SyntaxKind::RBracket,
            // Punctuation
            Token::Semicolon => SyntaxKind::Semicolon,
            Token::Comma => SyntaxKind::Comma,
            Token::Dot => SyntaxKind::Dot,
            Token::Colon => SyntaxKind::Colon,
            Token::Bang => SyntaxKind::Bang,
            // Operators
            Token::Equals => SyntaxKind::Equals,
            Token::Plus => SyntaxKind::Plus,
            Token::Minus => SyntaxKind::Minus,
            Token::Arrow => SyntaxKind::Arrow,
            Token::Star => SyntaxKind::Star,
            Token::Slash => SyntaxKind::Slash,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KestrelLanguage;

impl Language for KestrelLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        // Constants for pattern matching - suppress naming warnings
        const ROOT: u16 = SyntaxKind::Root as u16;
        const SOURCE_FILE: u16 = SyntaxKind::SourceFile as u16;
        const DECLARATION_ITEM: u16 = SyntaxKind::DeclarationItem as u16;
        const PROTOCOL_DECLARATION: u16 = SyntaxKind::ProtocolDeclaration as u16;
        const PROTOCOL_BODY: u16 = SyntaxKind::ProtocolBody as u16;
        const STRUCT_DECLARATION: u16 = SyntaxKind::StructDeclaration as u16;
        const STRUCT_BODY: u16 = SyntaxKind::StructBody as u16;
        const IMPORT_DECLARATION: u16 = SyntaxKind::ImportDeclaration as u16;
        const IMPORT_ITEM: u16 = SyntaxKind::ImportItem as u16;
        const MODULE_DECLARATION: u16 = SyntaxKind::ModuleDeclaration as u16;
        const MODULE_PATH: u16 = SyntaxKind::ModulePath as u16;
        const NAME: u16 = SyntaxKind::Name as u16;
        const TYPE_ALIAS_DECLARATION: u16 = SyntaxKind::TypeAliasDeclaration as u16;
        const ALIASED_TYPE: u16 = SyntaxKind::AliasedType as u16;
        const FIELD_DECLARATION: u16 = SyntaxKind::FieldDeclaration as u16;
        const FUNCTION_DECLARATION: u16 = SyntaxKind::FunctionDeclaration as u16;
        const FUNCTION_BODY: u16 = SyntaxKind::FunctionBody as u16;
        const PARAMETER_LIST: u16 = SyntaxKind::ParameterList as u16;
        const PARAMETER: u16 = SyntaxKind::Parameter as u16;
        const RETURN_TYPE: u16 = SyntaxKind::ReturnType as u16;
        const VISIBILITY: u16 = SyntaxKind::Visibility as u16;
        const STATIC_MODIFIER: u16 = SyntaxKind::StaticModifier as u16;
        const TY: u16 = SyntaxKind::Ty as u16;
        const TY_UNIT: u16 = SyntaxKind::TyUnit as u16;
        const TY_NEVER: u16 = SyntaxKind::TyNever as u16;
        const TY_TUPLE: u16 = SyntaxKind::TyTuple as u16;
        const TY_FUNCTION: u16 = SyntaxKind::TyFunction as u16;
        const TY_PATH: u16 = SyntaxKind::TyPath as u16;
        const TY_LIST: u16 = SyntaxKind::TyList as u16;
        const PATH: u16 = SyntaxKind::Path as u16;
        const PATH_ELEMENT: u16 = SyntaxKind::PathElement as u16;
        const IDENTIFIER: u16 = SyntaxKind::Identifier as u16;
        const STRING: u16 = SyntaxKind::String as u16;
        const INTEGER: u16 = SyntaxKind::Integer as u16;
        const FLOAT: u16 = SyntaxKind::Float as u16;
        const BOOLEAN: u16 = SyntaxKind::Boolean as u16;
        const NULL: u16 = SyntaxKind::Null as u16;
        const AS: u16 = SyntaxKind::As as u16;
        const ELSE: u16 = SyntaxKind::Else as u16;
        const FILEPRIVATE: u16 = SyntaxKind::Fileprivate as u16;
        const FUNC: u16 = SyntaxKind::Func as u16;
        const IF: u16 = SyntaxKind::If as u16;
        const IMPORT: u16 = SyntaxKind::Import as u16;
        const INTERNAL: u16 = SyntaxKind::Internal as u16;
        const LET: u16 = SyntaxKind::Let as u16;
        const MODULE: u16 = SyntaxKind::Module as u16;
        const PRIVATE: u16 = SyntaxKind::Private as u16;
        const PROTOCOL: u16 = SyntaxKind::Protocol as u16;
        const PUBLIC: u16 = SyntaxKind::Public as u16;
        const STATIC: u16 = SyntaxKind::Static as u16;
        const STRUCT: u16 = SyntaxKind::Struct as u16;
        const TYPE: u16 = SyntaxKind::Type as u16;
        const VAR: u16 = SyntaxKind::Var as u16;
        const LPAREN: u16 = SyntaxKind::LParen as u16;
        const RPAREN: u16 = SyntaxKind::RParen as u16;
        const LBRACE: u16 = SyntaxKind::LBrace as u16;
        const RBRACE: u16 = SyntaxKind::RBrace as u16;
        const LBRACKET: u16 = SyntaxKind::LBracket as u16;
        const RBRACKET: u16 = SyntaxKind::RBracket as u16;
        const SEMICOLON: u16 = SyntaxKind::Semicolon as u16;
        const COMMA: u16 = SyntaxKind::Comma as u16;
        const DOT: u16 = SyntaxKind::Dot as u16;
        const COLON: u16 = SyntaxKind::Colon as u16;
        const BANG: u16 = SyntaxKind::Bang as u16;
        const EQUALS: u16 = SyntaxKind::Equals as u16;
        const PLUS: u16 = SyntaxKind::Plus as u16;
        const MINUS: u16 = SyntaxKind::Minus as u16;
        const ARROW: u16 = SyntaxKind::Arrow as u16;
        const STAR: u16 = SyntaxKind::Star as u16;
        const SLASH: u16 = SyntaxKind::Slash as u16;
        const WHITESPACE: u16 = SyntaxKind::Whitespace as u16;
        const LINE_COMMENT: u16 = SyntaxKind::LineComment as u16;
        const BLOCK_COMMENT: u16 = SyntaxKind::BlockComment as u16;

        match raw.0 {
            ROOT => SyntaxKind::Root,
            SOURCE_FILE => SyntaxKind::SourceFile,
            DECLARATION_ITEM => SyntaxKind::DeclarationItem,
            PROTOCOL_DECLARATION => SyntaxKind::ProtocolDeclaration,
            PROTOCOL_BODY => SyntaxKind::ProtocolBody,
            STRUCT_DECLARATION => SyntaxKind::StructDeclaration,
            STRUCT_BODY => SyntaxKind::StructBody,
            IMPORT_DECLARATION => SyntaxKind::ImportDeclaration,
            IMPORT_ITEM => SyntaxKind::ImportItem,
            MODULE_DECLARATION => SyntaxKind::ModuleDeclaration,
            MODULE_PATH => SyntaxKind::ModulePath,
            NAME => SyntaxKind::Name,
            TYPE_ALIAS_DECLARATION => SyntaxKind::TypeAliasDeclaration,
            ALIASED_TYPE => SyntaxKind::AliasedType,
            FIELD_DECLARATION => SyntaxKind::FieldDeclaration,
            FUNCTION_DECLARATION => SyntaxKind::FunctionDeclaration,
            FUNCTION_BODY => SyntaxKind::FunctionBody,
            PARAMETER_LIST => SyntaxKind::ParameterList,
            PARAMETER => SyntaxKind::Parameter,
            RETURN_TYPE => SyntaxKind::ReturnType,
            VISIBILITY => SyntaxKind::Visibility,
            STATIC_MODIFIER => SyntaxKind::StaticModifier,
            TY => SyntaxKind::Ty,
            TY_UNIT => SyntaxKind::TyUnit,
            TY_NEVER => SyntaxKind::TyNever,
            TY_TUPLE => SyntaxKind::TyTuple,
            TY_FUNCTION => SyntaxKind::TyFunction,
            TY_PATH => SyntaxKind::TyPath,
            TY_LIST => SyntaxKind::TyList,
            PATH => SyntaxKind::Path,
            PATH_ELEMENT => SyntaxKind::PathElement,
            IDENTIFIER => SyntaxKind::Identifier,
            STRING => SyntaxKind::String,
            INTEGER => SyntaxKind::Integer,
            FLOAT => SyntaxKind::Float,
            BOOLEAN => SyntaxKind::Boolean,
            NULL => SyntaxKind::Null,
            AS => SyntaxKind::As,
            ELSE => SyntaxKind::Else,
            FILEPRIVATE => SyntaxKind::Fileprivate,
            FUNC => SyntaxKind::Func,
            IF => SyntaxKind::If,
            IMPORT => SyntaxKind::Import,
            INTERNAL => SyntaxKind::Internal,
            LET => SyntaxKind::Let,
            MODULE => SyntaxKind::Module,
            PRIVATE => SyntaxKind::Private,
            PROTOCOL => SyntaxKind::Protocol,
            PUBLIC => SyntaxKind::Public,
            STATIC => SyntaxKind::Static,
            STRUCT => SyntaxKind::Struct,
            TYPE => SyntaxKind::Type,
            VAR => SyntaxKind::Var,
            LPAREN => SyntaxKind::LParen,
            RPAREN => SyntaxKind::RParen,
            LBRACE => SyntaxKind::LBrace,
            RBRACE => SyntaxKind::RBrace,
            LBRACKET => SyntaxKind::LBracket,
            RBRACKET => SyntaxKind::RBracket,
            SEMICOLON => SyntaxKind::Semicolon,
            COMMA => SyntaxKind::Comma,
            DOT => SyntaxKind::Dot,
            COLON => SyntaxKind::Colon,
            BANG => SyntaxKind::Bang,
            EQUALS => SyntaxKind::Equals,
            PLUS => SyntaxKind::Plus,
            MINUS => SyntaxKind::Minus,
            ARROW => SyntaxKind::Arrow,
            STAR => SyntaxKind::Star,
            SLASH => SyntaxKind::Slash,
            WHITESPACE => SyntaxKind::Whitespace,
            LINE_COMMENT => SyntaxKind::LineComment,
            BLOCK_COMMENT => SyntaxKind::BlockComment,
            _ => SyntaxKind::Error,
        }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<KestrelLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<KestrelLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<KestrelLanguage>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_syntax_kind_conversion() {
        // Test that Token to SyntaxKind conversion works
        assert_eq!(SyntaxKind::from(kestrel_lexer::Token::Module), SyntaxKind::Module);
        assert_eq!(SyntaxKind::from(kestrel_lexer::Token::Identifier), SyntaxKind::Identifier);
        assert_eq!(SyntaxKind::from(kestrel_lexer::Token::Dot), SyntaxKind::Dot);
    }

    #[test]
    fn test_basic_tree() {
        // Test building a simple syntax tree
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::Root.into());
        builder.token(SyntaxKind::Identifier.into(), "test");
        builder.finish_node();

        let green = builder.finish();
        let root = SyntaxNode::new_root(green);

        assert_eq!(root.kind(), SyntaxKind::Root);
    }
}
