//! Shared data structures used across multiple parser modules
//!
//! This module contains data types that are used by multiple parsers
//! to avoid duplication and ensure consistency.

use kestrel_lexer::Token;
use kestrel_span::Span;

use crate::ty::TyVariant;
use crate::type_param::{TypeParameterData, WhereClauseData};
use crate::block::CodeBlockData;

/// Raw parsed data for a single parameter
///
/// Parameter syntax: `(label)? bind_name: Type`
/// - `label` is an optional external parameter name (used by callers)
/// - `bind_name` is the internal parameter name (used in function body)
/// - If only one name is provided, it's used as both label and bind_name
///
/// # Examples
/// - `x: Int` → label=None, bind_name=x
/// - `with x: Int` → label="with", bind_name=x
#[derive(Debug, Clone)]
pub struct ParameterData {
    /// Optional label (external name for callers)
    /// If None, bind_name is used as the label
    pub label: Option<Span>,
    /// The binding name (internal name used in function body)
    pub bind_name: Span,
    /// The colon span
    pub colon: Span,
    /// The parameter type
    pub ty: TyVariant,
}

/// Raw parsed data for function declaration internals
///
/// Used by both function declarations and protocol method declarations.
#[derive(Debug, Clone)]
pub struct FunctionDeclarationData {
    pub visibility: Option<(Token, Span)>,
    pub is_static: Option<Span>,
    pub fn_span: Span,
    pub name_span: Span,
    pub type_params: Option<(Span, Vec<TypeParameterData>, Span)>,
    pub lparen: Span,
    pub parameters: Vec<ParameterData>,
    pub rparen: Span,
    pub return_type: Option<(Span, TyVariant)>, // (arrow_span, return_ty)
    pub where_clause: Option<WhereClauseData>,
    pub body: Option<CodeBlockData>, // Optional code block - None for protocol methods
}

/// Raw parsed data for field declaration internals
#[derive(Debug, Clone)]
pub struct FieldDeclarationData {
    pub visibility: Option<(Token, Span)>,
    pub is_static: Option<Span>,
    pub mutability_span: Span,
    pub is_mutable: bool,
    pub name_span: Span,
    pub colon_span: Span,
    pub ty: TyVariant,
}

/// Raw parsed data for struct declaration internals
#[derive(Debug, Clone)]
pub struct StructDeclarationData {
    pub visibility: Option<(Token, Span)>,
    pub struct_span: Span,
    pub name_span: Span,
    pub type_params: Option<(Span, Vec<TypeParameterData>, Span)>,
    pub where_clause: Option<WhereClauseData>,
    pub lbrace_span: Span,
    pub body: Vec<StructBodyItem>,
    pub rbrace_span: Span,
}

/// Items that can appear in a struct body
#[derive(Debug, Clone)]
pub enum StructBodyItem {
    Field(FieldDeclarationData),
    Function(FunctionDeclarationData),
    Struct(StructDeclarationData),
    Module(Span, Vec<Span>), // module_span, path_segments
    Import(Span, Vec<Span>, Option<Span>, Option<Vec<(Span, Option<Span>)>>), // import_span, path, alias, items
}

/// Raw parsed data for protocol declaration internals
#[derive(Debug, Clone)]
pub struct ProtocolDeclarationData {
    pub visibility: Option<(Token, Span)>,
    pub protocol_span: Span,
    pub name_span: Span,
    pub type_params: Option<(Span, Vec<TypeParameterData>, Span)>,
    pub where_clause: Option<WhereClauseData>,
    pub lbrace_span: Span,
    pub body: Vec<FunctionDeclarationData>, // Protocol body only contains function declarations
    pub rbrace_span: Span,
}

/// Raw parsed data for type alias declaration internals
#[derive(Debug, Clone)]
pub struct TypeAliasDeclarationData {
    pub visibility: Option<(Token, Span)>,
    pub type_span: Span,
    pub name_span: Span,
    pub type_params: Option<(Span, Vec<TypeParameterData>, Span)>,
    pub equals_span: Span,
    pub aliased_type: TyVariant,
    pub semicolon_span: Span,
}
