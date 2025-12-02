//! Function body resolution
//!
//! This module handles resolution of function bodies, including:
//! - Binding function parameters as locals
//! - Processing variable declarations (let/var)
//! - Resolving path expressions to locals or symbols
//! - Managing scopes for nested blocks

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior_ext::SymbolBehaviorExt;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::expression::{ExprKind, ExpressionSymbol};
use kestrel_semantic_tree::symbol::function::FunctionSymbol;
use kestrel_semantic_tree::symbol::local::LocalId;
use kestrel_semantic_tree::ty::Ty;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::local_scope::LocalScope;
use crate::queries::{Db, ValuePathResolution};
use crate::type_syntax::{resolve_type_from_ty_node, TypeSyntaxContext};
use crate::utils::get_node_span;

/// Context for body resolution
pub struct BodyResolutionContext<'a> {
    /// Database for queries
    pub db: &'a dyn Db,
    /// Diagnostics for error reporting
    pub diagnostics: &'a mut DiagnosticContext,
    /// File ID for error reporting
    pub file_id: usize,
    /// Source code
    pub source: &'a str,
}

/// Result of resolving a path expression
#[derive(Debug, Clone)]
pub enum ResolvedPath {
    /// Resolved to a local variable
    Local {
        function_id: SymbolId,
        local_id: LocalId,
        ty: Ty,
    },
    /// Resolved to a symbol (function, field, etc.)
    Symbol {
        symbol_id: SymbolId,
        ty: Ty,
    },
    /// Resolved to overloaded functions (needs call context to disambiguate)
    Overloaded {
        candidates: Vec<SymbolId>,
    },
    /// Resolution failed
    Error,
}

/// Resolve a function body
///
/// This binds parameters as locals and processes the body statements.
pub fn resolve_function_body(
    function: &Arc<FunctionSymbol>,
    body_node: &SyntaxNode,
    context: &mut BodyResolutionContext,
) {
    let function_id = function.metadata().id();

    // Create local scope for this function
    let mut local_scope = LocalScope::new(function.clone());

    // Bind function parameters as locals
    bind_parameters_as_locals(function, &mut local_scope, context);

    // Process the body
    resolve_body_contents(body_node, &mut local_scope, function_id, context);
}

/// Bind function parameters as local variables
fn bind_parameters_as_locals(
    function: &Arc<FunctionSymbol>,
    local_scope: &mut LocalScope,
    _context: &BodyResolutionContext,
) {
    // Get the CallableBehavior to access resolved parameter types
    if let Some(callable) = function.callable_behavior() {
        for param in callable.parameters() {
            let name = param.bind_name.value.clone();
            let ty = param.ty.clone();
            let span = param.bind_name.span.clone();

            // Parameters are immutable by default
            local_scope.bind(name, ty, false, span);
        }
    }
}

/// Resolve the contents of a function body
fn resolve_body_contents(
    body_node: &SyntaxNode,
    local_scope: &mut LocalScope,
    function_id: SymbolId,
    context: &mut BodyResolutionContext,
) {
    // FunctionBody contains statements
    for child in body_node.children() {
        match child.kind() {
            SyntaxKind::Statement => {
                resolve_statement(&child, local_scope, function_id, context);
            }
            SyntaxKind::VariableDeclaration => {
                resolve_variable_declaration(&child, local_scope, function_id, context);
            }
            SyntaxKind::ExpressionStatement => {
                resolve_expression_statement(&child, local_scope, function_id, context);
            }
            _ => {
                // Recurse into other nodes that might contain statements
                resolve_body_contents(&child, local_scope, function_id, context);
            }
        }
    }
}

/// Resolve a statement
fn resolve_statement(
    stmt_node: &SyntaxNode,
    local_scope: &mut LocalScope,
    function_id: SymbolId,
    context: &mut BodyResolutionContext,
) {
    // A Statement wraps a specific statement type
    for child in stmt_node.children() {
        match child.kind() {
            SyntaxKind::VariableDeclaration => {
                resolve_variable_declaration(&child, local_scope, function_id, context);
            }
            SyntaxKind::ExpressionStatement => {
                resolve_expression_statement(&child, local_scope, function_id, context);
            }
            _ => {}
        }
    }
}

/// Resolve a variable declaration (let/var)
fn resolve_variable_declaration(
    var_decl_node: &SyntaxNode,
    local_scope: &mut LocalScope,
    function_id: SymbolId,
    context: &mut BodyResolutionContext,
) {
    // Check mutability
    let is_mutable = var_decl_node
        .children_with_tokens()
        .any(|elem| elem.kind() == SyntaxKind::Var);

    // Extract name
    let name_node = var_decl_node
        .children()
        .find(|c| c.kind() == SyntaxKind::Name);

    let Some(name_node) = name_node else {
        return;
    };

    let name_span = get_node_span(&name_node, context.source);
    let name = name_node
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Identifier)
        .map(|t| t.text().to_string())
        .unwrap_or_default();

    if name.is_empty() {
        return;
    }

    // Extract type annotation if present
    let ty = extract_type_annotation(var_decl_node, function_id, context);

    // If there's an initializer expression, resolve it
    if let Some(expr_node) = find_expression_in_decl(var_decl_node) {
        let _resolved_expr = resolve_expression(&expr_node, local_scope, function_id, context);
        // TODO: Type inference from initializer if no annotation
    }

    // Bind the variable
    local_scope.bind(name, ty, is_mutable, name_span);
}

/// Extract type annotation from a variable declaration
fn extract_type_annotation(
    var_decl_node: &SyntaxNode,
    function_id: SymbolId,
    context: &mut BodyResolutionContext,
) -> Ty {
    // Look for Ty node after colon
    let ty_node = var_decl_node
        .children()
        .find(|c| c.kind() == SyntaxKind::Ty);

    if let Some(ty_node) = ty_node {
        return resolve_type_from_syntax(&ty_node, function_id, context);
    }

    // No type annotation - return inferred type placeholder
    let span = get_node_span(var_decl_node, context.source);
    Ty::inferred(span)
}

/// Find the initializer expression in a variable declaration
fn find_expression_in_decl(var_decl_node: &SyntaxNode) -> Option<SyntaxNode> {
    var_decl_node
        .children()
        .find(|c| c.kind() == SyntaxKind::Expression)
}

/// Resolve an expression statement
fn resolve_expression_statement(
    expr_stmt_node: &SyntaxNode,
    local_scope: &mut LocalScope,
    function_id: SymbolId,
    context: &mut BodyResolutionContext,
) {
    // Find the expression inside
    if let Some(expr_node) = expr_stmt_node
        .children()
        .find(|c| c.kind() == SyntaxKind::Expression)
    {
        let _resolved = resolve_expression(&expr_node, local_scope, function_id, context);
    }
}

/// Resolve an expression, returning the resolved expression symbol
pub fn resolve_expression(
    expr_node: &SyntaxNode,
    local_scope: &LocalScope,
    function_id: SymbolId,
    context: &mut BodyResolutionContext,
) -> Option<Arc<ExpressionSymbol>> {
    let span = get_node_span(expr_node, context.source);

    // Find the specific expression type
    let expr_child = expr_node.children().next()?;

    match expr_child.kind() {
        SyntaxKind::ExprPath => {
            resolve_path_expression(&expr_child, local_scope, function_id, context)
        }
        SyntaxKind::ExprInteger => {
            // Already handled in expression resolver - just create the symbol
            let text = expr_child
                .children_with_tokens()
                .filter_map(|e| e.into_token())
                .find(|t| t.kind() == SyntaxKind::Integer)
                .map(|t| t.text().to_string())?;

            let value = parse_integer(&text)?;
            Some(Arc::new(ExpressionSymbol::integer(value, span, None)))
        }
        SyntaxKind::ExprFloat => {
            let text = expr_child
                .children_with_tokens()
                .filter_map(|e| e.into_token())
                .find(|t| t.kind() == SyntaxKind::Float)
                .map(|t| t.text().to_string())?;

            let value: f64 = text.replace('_', "").parse().ok()?;
            Some(Arc::new(ExpressionSymbol::float(value, span, None)))
        }
        SyntaxKind::ExprString => {
            let text = expr_child
                .children_with_tokens()
                .filter_map(|e| e.into_token())
                .find(|t| t.kind() == SyntaxKind::String)
                .map(|t| {
                    let s = t.text();
                    // Remove quotes
                    s[1..s.len() - 1].to_string()
                })?;

            Some(Arc::new(ExpressionSymbol::string(text, span, None)))
        }
        SyntaxKind::ExprBool => {
            let text = expr_child
                .children_with_tokens()
                .filter_map(|e| e.into_token())
                .find(|t| t.kind() == SyntaxKind::Boolean)
                .map(|t| t.text().to_string())?;

            let value = text == "true";
            Some(Arc::new(ExpressionSymbol::bool(value, span, None)))
        }
        SyntaxKind::ExprUnit => Some(Arc::new(ExpressionSymbol::unit(span, None))),
        SyntaxKind::ExprGrouping => {
            // Resolve inner expression
            if let Some(inner_expr) = expr_child.children().find(|c| c.kind() == SyntaxKind::Expression) {
                if let Some(inner) = resolve_expression(&inner_expr, local_scope, function_id, context) {
                    return Some(Arc::new(ExpressionSymbol::grouping(inner, span, None)));
                }
            }
            None
        }
        SyntaxKind::ExprArray => {
            // Resolve element expressions
            let elements: Vec<Arc<ExpressionSymbol>> = expr_child
                .children()
                .filter(|c| c.kind() == SyntaxKind::Expression)
                .filter_map(|e| resolve_expression(&e, local_scope, function_id, context))
                .collect();
            // Infer element type from first element if available
            let element_ty = elements.first().and_then(|e| e.ty().cloned());
            Some(Arc::new(ExpressionSymbol::array(elements, span, element_ty, None)))
        }
        SyntaxKind::ExprTuple => {
            // Resolve element expressions
            let elements: Vec<Arc<ExpressionSymbol>> = expr_child
                .children()
                .filter(|c| c.kind() == SyntaxKind::Expression)
                .filter_map(|e| resolve_expression(&e, local_scope, function_id, context))
                .collect();
            Some(Arc::new(ExpressionSymbol::tuple(elements, span, None)))
        }
        _ => None,
    }
}

/// Resolve a path expression, checking locals first, then module scope
fn resolve_path_expression(
    path_node: &SyntaxNode,
    local_scope: &LocalScope,
    function_id: SymbolId,
    context: &BodyResolutionContext,
) -> Option<Arc<ExpressionSymbol>> {
    let span = get_node_span(path_node, context.source);

    // Extract path segments
    let segments: Vec<(String, kestrel_span::Span)> = path_node
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| t.kind() == SyntaxKind::Identifier)
        .map(|t| {
            let range = t.text_range();
            let seg_span = range.start().into()..range.end().into();
            (t.text().to_string(), seg_span)
        })
        .collect();

    if segments.is_empty() {
        return None;
    }

    // For single-segment paths, check locals first
    if segments.len() == 1 {
        let name = &segments[0].0;

        // Check local scope
        if let Some(local_id) = local_scope.lookup(name) {
            // Get the local's type from the function
            if let Some(local) = local_scope.function().get_local(local_id) {
                let ty = local.ty().clone();
                return Some(Arc::new(ExpressionSymbol::local_ref(
                    name.clone(),
                    function_id,
                    local_id,
                    ty,
                    span,
                    None,
                )));
            }
        }
    }

    // Fall back to module scope resolution
    let path_strings: Vec<String> = segments.iter().map(|(s, _)| s.clone()).collect();
    let resolution = context.db.resolve_value_path(path_strings.clone(), function_id);

    match resolution {
        ValuePathResolution::Symbol { symbol_id, ty } => {
            let name = path_strings.join(".");
            Some(Arc::new(ExpressionSymbol::symbol_ref(
                name,
                symbol_id,
                ty,
                span,
                None,
            )))
        }
        ValuePathResolution::Overloaded { candidates } => {
            // For now, just create a path expression - call resolution will disambiguate
            Some(Arc::new(ExpressionSymbol::path(segments, span, None)))
        }
        ValuePathResolution::NotFound { segment, .. } => {
            // TODO: Report error for unresolved path
            None
        }
        ValuePathResolution::Ambiguous { .. } | ValuePathResolution::NotAValue { .. } => {
            // TODO: Report error
            None
        }
    }
}

/// Parse an integer literal (handles hex, binary, octal)
fn parse_integer(text: &str) -> Option<i64> {
    let text = text.replace('_', "");

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

/// Resolve a type from a Ty syntax node
///
/// This extracts the type from syntax and immediately resolves it using the database.
/// Returns an error type if resolution fails.
fn resolve_type_from_syntax(
    ty_node: &SyntaxNode,
    context_id: SymbolId,
    ctx: &mut BodyResolutionContext,
) -> Ty {
    let mut type_ctx = TypeSyntaxContext {
        db: ctx.db,
        diagnostics: ctx.diagnostics,
        file_id: ctx.file_id,
        source: ctx.source,
        context_id,
    };
    resolve_type_from_ty_node(ty_node, &mut type_ctx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer_decimal() {
        assert_eq!(parse_integer("42"), Some(42));
        assert_eq!(parse_integer("1_000_000"), Some(1_000_000));
    }

    #[test]
    fn test_parse_integer_hex() {
        assert_eq!(parse_integer("0xFF"), Some(255));
        assert_eq!(parse_integer("0x1A_2B"), Some(0x1A2B));
    }

    #[test]
    fn test_parse_integer_binary() {
        assert_eq!(parse_integer("0b1010"), Some(10));
        assert_eq!(parse_integer("0b1111_0000"), Some(0b11110000));
    }

    #[test]
    fn test_parse_integer_octal() {
        assert_eq!(parse_integer("0o17"), Some(15));
        assert_eq!(parse_integer("0o77_77"), Some(0o7777));
    }
}
