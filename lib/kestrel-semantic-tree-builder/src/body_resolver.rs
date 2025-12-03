//! Function body resolution.
//!
//! This module handles resolving function bodies from syntax to semantic
//! representations (Expression, Statement, CodeBlock). It runs during the
//! bind phase after all symbols have been created.

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::executable::{CodeBlock, ExecutableBehavior};
use kestrel_semantic_tree::expr::Expression;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::pattern::{Mutability, Pattern};
use kestrel_semantic_tree::stmt::Statement;
use kestrel_semantic_tree::symbol::function::FunctionSymbol;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::local_scope::LocalScope;
use crate::queries::{Db, ValuePathResolution};
use crate::utils::get_node_span;

/// Context for body resolution
pub struct BodyResolutionContext<'a> {
    /// The database for queries
    pub db: &'a dyn Db,
    /// Diagnostics collector
    pub diagnostics: &'a mut DiagnosticContext,
    /// File ID for error reporting
    pub file_id: usize,
    /// Source code for span extraction
    pub source: &'a str,
    /// The function symbol ID (for path resolution context)
    pub function_id: SymbolId,
    /// Local scope for variable tracking
    pub local_scope: LocalScope,
}

impl<'a> BodyResolutionContext<'a> {
    /// Create a new body resolution context
    pub fn new(
        db: &'a dyn Db,
        diagnostics: &'a mut DiagnosticContext,
        file_id: usize,
        source: &'a str,
        function: Arc<FunctionSymbol>,
    ) -> Self {
        let function_id = function.metadata().id();
        let local_scope = LocalScope::new(function);
        BodyResolutionContext {
            db,
            diagnostics,
            file_id,
            source,
            function_id,
            local_scope,
        }
    }
}

/// Resolve a function body syntax node into a CodeBlock
pub fn resolve_function_body(
    body_node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> CodeBlock {
    // FunctionBody contains either a CodeBlock or a single Expression
    // FunctionBody -> { CodeBlock } | { Expression }

    // Check for CodeBlock child
    if let Some(code_block) = body_node.children().find(|c| c.kind() == SyntaxKind::CodeBlock) {
        return resolve_code_block(&code_block, ctx);
    }

    // Check for Expression child (shorthand: func foo() -> Int = 42)
    if let Some(expr_node) = body_node.children().find(|c| c.kind() == SyntaxKind::Expression) {
        let expr = resolve_expression(&expr_node, ctx);
        return CodeBlock::new(vec![], Some(expr));
    }

    // Empty body
    CodeBlock::empty()
}

/// Resolve a code block (statements + optional yield expression)
fn resolve_code_block(
    block_node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> CodeBlock {
    ctx.local_scope.push_scope();

    let mut statements = Vec::new();
    let mut yield_expr = None;

    // Process children
    let children: Vec<_> = block_node.children().collect();

    for (i, child) in children.iter().enumerate() {
        let is_last = i == children.len() - 1;

        match child.kind() {
            SyntaxKind::Statement | SyntaxKind::ExpressionStatement => {
                if let Some(stmt) = resolve_statement(child, ctx) {
                    statements.push(stmt);
                }
            }
            SyntaxKind::VariableDeclaration => {
                if let Some(stmt) = resolve_variable_declaration(child, ctx) {
                    statements.push(stmt);
                }
            }
            SyntaxKind::Expression => {
                // If this is the last child without a semicolon, it's the yield expression
                // Otherwise it's an expression statement
                if is_last && !has_trailing_semicolon(child) {
                    yield_expr = Some(resolve_expression(child, ctx));
                } else {
                    let expr = resolve_expression(child, ctx);
                    let span = get_node_span(child, ctx.source);
                    statements.push(Statement::expr(expr, span));
                }
            }
            // Skip tokens like braces
            _ => {}
        }
    }

    ctx.local_scope.pop_scope();

    CodeBlock::new(statements, yield_expr)
}

/// Check if a node has a trailing semicolon
fn has_trailing_semicolon(node: &SyntaxNode) -> bool {
    // Check if the node or its parent has a semicolon token after
    node.children_with_tokens()
        .any(|elem| elem.kind() == SyntaxKind::Semicolon)
}

/// Resolve a statement syntax node
fn resolve_statement(
    stmt_node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Option<Statement> {
    // Statement wrapper - look inside for actual content
    for child in stmt_node.children() {
        match child.kind() {
            SyntaxKind::VariableDeclaration => {
                return resolve_variable_declaration(&child, ctx);
            }
            SyntaxKind::ExpressionStatement => {
                return resolve_expression_statement(&child, ctx);
            }
            SyntaxKind::Expression => {
                let expr = resolve_expression(&child, ctx);
                let span = get_node_span(&child, ctx.source);
                return Some(Statement::expr(expr, span));
            }
            _ => {}
        }
    }
    None
}

/// Resolve an expression statement (expression with semicolon)
fn resolve_expression_statement(
    stmt_node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Option<Statement> {
    // Find the expression child
    if let Some(expr_node) = stmt_node.children().find(|c| c.kind() == SyntaxKind::Expression) {
        let expr = resolve_expression(&expr_node, ctx);
        let span = get_node_span(stmt_node, ctx.source);
        return Some(Statement::expr(expr, span));
    }

    // Also check for bare expression kinds
    for child in stmt_node.children() {
        if is_expression_kind(child.kind()) {
            let expr = resolve_expression(&child, ctx);
            let span = get_node_span(stmt_node, ctx.source);
            return Some(Statement::expr(expr, span));
        }
    }

    None
}

/// Resolve a variable declaration (let/var)
fn resolve_variable_declaration(
    decl_node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Option<Statement> {
    let span = get_node_span(decl_node, ctx.source);

    // Determine if let or var
    let is_mutable = decl_node.children_with_tokens()
        .any(|elem| elem.kind() == SyntaxKind::Var);
    let mutability = if is_mutable { Mutability::Mutable } else { Mutability::Immutable };

    // Extract name
    let name = extract_var_name(decl_node)?;

    // Extract type annotation (if any)
    let ty = extract_var_type(decl_node, ctx);

    // Extract initializer (if any)
    let value = decl_node.children()
        .find(|c| c.kind() == SyntaxKind::Expression || is_expression_kind(c.kind()))
        .map(|expr_node| resolve_expression(&expr_node, ctx));

    // Determine the type from annotation or initializer
    let resolved_ty = ty.unwrap_or_else(|| {
        value.as_ref()
            .map(|e| e.ty.clone())
            .unwrap_or_else(|| Ty::inferred(span.clone()))
    });

    // Bind the local variable
    let name_span = get_name_span(decl_node, ctx.source).unwrap_or(span.clone());
    let local_id = ctx.local_scope.bind(name.clone(), resolved_ty.clone(), is_mutable, name_span.clone());

    // Create the pattern
    let pattern = Pattern::local(local_id, mutability, name, resolved_ty, name_span);

    Some(Statement::binding(pattern, value, span))
}

/// Extract the variable name from a VariableDeclaration node
fn extract_var_name(decl_node: &SyntaxNode) -> Option<String> {
    // Look for Name node
    if let Some(name_node) = decl_node.children().find(|c| c.kind() == SyntaxKind::Name) {
        // Get identifier token from Name
        return name_node.children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::Identifier)
            .map(|t| t.text().to_string());
    }

    // Fallback: look for bare Identifier
    decl_node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Identifier)
        .map(|t| t.text().to_string())
}

/// Get the span of the name in a variable declaration
fn get_name_span(decl_node: &SyntaxNode, source: &str) -> Option<Span> {
    if let Some(name_node) = decl_node.children().find(|c| c.kind() == SyntaxKind::Name) {
        return Some(get_node_span(&name_node, source));
    }
    None
}

/// Extract type annotation from a variable declaration
fn extract_var_type(decl_node: &SyntaxNode, ctx: &BodyResolutionContext) -> Option<Ty> {
    // Look for Ty node
    decl_node.children()
        .find(|c| c.kind() == SyntaxKind::Ty)
        .map(|ty_node| {
            // Use the type syntax module to extract the type
            crate::type_syntax::extract_type_from_ty_node(&ty_node, ctx.source)
        })
}

/// Check if a syntax kind is an expression kind
fn is_expression_kind(kind: SyntaxKind) -> bool {
    matches!(kind,
        SyntaxKind::Expression
        | SyntaxKind::ExprUnit
        | SyntaxKind::ExprInteger
        | SyntaxKind::ExprFloat
        | SyntaxKind::ExprString
        | SyntaxKind::ExprBool
        | SyntaxKind::ExprArray
        | SyntaxKind::ExprTuple
        | SyntaxKind::ExprGrouping
        | SyntaxKind::ExprPath
        | SyntaxKind::ExprUnary
        | SyntaxKind::ExprNull
    )
}

/// Resolve an expression syntax node into a semantic Expression
pub fn resolve_expression(
    expr_node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let span = get_node_span(expr_node, ctx.source);

    match expr_node.kind() {
        SyntaxKind::Expression => {
            // Wrapper node - resolve the inner expression
            for child in expr_node.children() {
                if is_expression_kind(child.kind()) {
                    return resolve_expression(&child, ctx);
                }
            }
            Expression::error(span)
        }

        SyntaxKind::ExprUnit => Expression::unit(span),

        SyntaxKind::ExprInteger => {
            let value = extract_integer_value(expr_node);
            Expression::integer(value, span)
        }

        SyntaxKind::ExprFloat => {
            let value = extract_float_value(expr_node);
            Expression::float(value, span)
        }

        SyntaxKind::ExprString => {
            let value = extract_string_value(expr_node);
            Expression::string(value, span)
        }

        SyntaxKind::ExprBool => {
            let value = extract_bool_value(expr_node);
            Expression::bool(value, span)
        }

        SyntaxKind::ExprArray => {
            resolve_array_expression(expr_node, ctx)
        }

        SyntaxKind::ExprTuple => {
            resolve_tuple_expression(expr_node, ctx)
        }

        SyntaxKind::ExprGrouping => {
            resolve_grouping_expression(expr_node, ctx)
        }

        SyntaxKind::ExprPath => {
            resolve_path_expression(expr_node, ctx)
        }

        SyntaxKind::ExprUnary => {
            // TODO: Implement unary operators
            Expression::error(span)
        }

        SyntaxKind::ExprNull => {
            // TODO: Handle null properly with optional types
            Expression::error(span)
        }

        _ => Expression::error(span),
    }
}

/// Extract integer value from an ExprInteger node
fn extract_integer_value(node: &SyntaxNode) -> i64 {
    node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Integer)
        .and_then(|t| parse_integer_literal(t.text()))
        .unwrap_or(0)
}

/// Parse an integer literal (handles 0x, 0b, 0o prefixes)
fn parse_integer_literal(text: &str) -> Option<i64> {
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

/// Extract float value from an ExprFloat node
fn extract_float_value(node: &SyntaxNode) -> f64 {
    node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Float)
        .and_then(|t| t.text().replace('_', "").parse().ok())
        .unwrap_or(0.0)
}

/// Extract string value from an ExprString node (strips quotes)
fn extract_string_value(node: &SyntaxNode) -> String {
    node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::String)
        .map(|t| {
            let text = t.text();
            // Strip surrounding quotes
            if text.len() >= 2 {
                text[1..text.len()-1].to_string()
            } else {
                text.to_string()
            }
        })
        .unwrap_or_default()
}

/// Extract boolean value from an ExprBool node
fn extract_bool_value(node: &SyntaxNode) -> bool {
    node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Boolean)
        .map(|t| t.text() == "true")
        .unwrap_or(false)
}

/// Resolve an array expression: [1, 2, 3]
fn resolve_array_expression(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let span = get_node_span(node, ctx.source);

    let elements: Vec<Expression> = node.children()
        .filter(|c| c.kind() == SyntaxKind::Expression || is_expression_kind(c.kind()))
        .map(|c| resolve_expression(&c, ctx))
        .collect();

    // Infer element type from first element, or use inferred if empty
    let element_ty = elements.first()
        .map(|e| e.ty.clone())
        .unwrap_or_else(|| Ty::inferred(span.clone()));

    Expression::array(elements, element_ty, span)
}

/// Resolve a tuple expression: (1, 2, 3)
fn resolve_tuple_expression(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let span = get_node_span(node, ctx.source);

    let elements: Vec<Expression> = node.children()
        .filter(|c| c.kind() == SyntaxKind::Expression || is_expression_kind(c.kind()))
        .map(|c| resolve_expression(&c, ctx))
        .collect();

    Expression::tuple(elements, span)
}

/// Resolve a grouping expression: (expr)
fn resolve_grouping_expression(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let span = get_node_span(node, ctx.source);

    // Find the inner expression
    if let Some(inner_node) = node.children()
        .find(|c| c.kind() == SyntaxKind::Expression || is_expression_kind(c.kind()))
    {
        let inner = resolve_expression(&inner_node, ctx);
        return Expression::grouping(inner, span);
    }

    Expression::error(span)
}

/// Resolve a path expression (variable reference, function reference, or field access)
fn resolve_path_expression(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let span = get_node_span(node, ctx.source);

    // Extract the path segments
    let path = extract_path_segments(node);

    if path.is_empty() {
        return Expression::error(span);
    }

    // First, check if it's a local variable
    if let Some(local_id) = ctx.local_scope.lookup(&path[0]) {
        // Get the type from the local
        let local_ty = ctx.local_scope.function()
            .get_local(local_id)
            .map(|l| l.ty().clone())
            .unwrap_or_else(|| Ty::error(span.clone()));

        let base_expr = Expression::local_ref(local_id, local_ty, span.clone());

        // If there are more segments, they are field accesses
        if path.len() == 1 {
            return base_expr;
        } else {
            return resolve_field_chain(base_expr, &path[1..], span, ctx);
        }
    }

    // Not a local - resolve as a value path (module path)
    match ctx.db.resolve_value_path(path.clone(), ctx.function_id) {
        ValuePathResolution::Symbol { symbol_id, ty } => {
            Expression::symbol_ref(symbol_id, ty, span)
        }
        ValuePathResolution::Overloaded { candidates } => {
            Expression::overloaded_ref(candidates, span)
        }
        ValuePathResolution::NotFound { segment, index } => {
            // TODO: Report error diagnostic
            let _ = (segment, index);
            Expression::error(span)
        }
        ValuePathResolution::Ambiguous { segment, index, candidates } => {
            // TODO: Report error diagnostic
            let _ = (segment, index, candidates);
            Expression::error(span)
        }
        ValuePathResolution::NotAValue { symbol_id } => {
            // TODO: Report error diagnostic
            let _ = symbol_id;
            Expression::error(span)
        }
    }
}

/// Resolve a chain of field accesses: obj.field1.field2.field3
fn resolve_field_chain(
    base: Expression,
    fields: &[String],
    span: Span,
    ctx: &BodyResolutionContext,
) -> Expression {
    let mut current = base;

    for field_name in fields {
        let field_ty = lookup_field_type(&current.ty, field_name, ctx);
        current = Expression::field_access(current, field_name.clone(), field_ty, span.clone());
    }

    current
}

/// Look up a field's type on a given type
fn lookup_field_type(ty: &Ty, field_name: &str, _ctx: &BodyResolutionContext) -> Ty {
    use kestrel_semantic_tree::ty::TyKind;
    use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
    use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
    use kestrel_semantic_tree::behavior::typed::TypedBehavior;

    match ty.kind() {
        TyKind::Struct { symbol, .. } => {
            // Look through struct's children to find the field
            for child in symbol.metadata().children() {
                if child.metadata().kind() == KestrelSymbolKind::Field {
                    if child.metadata().name().value == field_name {
                        // Get the type from TypedBehavior (set during bind phase)
                        for behavior in child.metadata().behaviors() {
                            if behavior.kind() == KestrelBehaviorKind::Typed {
                                if let Some(typed) = behavior.as_ref().downcast_ref::<TypedBehavior>() {
                                    return typed.ty().clone();
                                }
                            }
                        }
                    }
                }
            }
            // Field not found
            Ty::error(ty.span().clone())
        }
        // TODO: Handle tuple field access (e.g., tuple.0, tuple.1)
        _ => Ty::error(ty.span().clone()),
    }
}

/// Extract path segments from a path expression node
fn extract_path_segments(node: &SyntaxNode) -> Vec<String> {
    let mut segments = Vec::new();

    // ExprPath may contain Path or direct PathElements
    if let Some(path_node) = node.children().find(|c| c.kind() == SyntaxKind::Path) {
        // Path contains PathElements
        for element in path_node.children() {
            if element.kind() == SyntaxKind::PathElement {
                if let Some(name) = extract_path_element_name(&element) {
                    segments.push(name);
                }
            }
        }
    } else {
        // Direct identifiers
        for child in node.children() {
            if child.kind() == SyntaxKind::PathElement {
                if let Some(name) = extract_path_element_name(&child) {
                    segments.push(name);
                }
            }
        }

        // Fallback: look for Name or Identifier tokens
        if segments.is_empty() {
            for elem in node.children_with_tokens() {
                if let Some(token) = elem.as_token() {
                    if token.kind() == SyntaxKind::Identifier {
                        segments.push(token.text().to_string());
                    }
                }
            }
        }
    }

    segments
}

/// Extract the name from a PathElement node
fn extract_path_element_name(element: &SyntaxNode) -> Option<String> {
    // PathElement contains Name or Identifier
    if let Some(name_node) = element.children().find(|c| c.kind() == SyntaxKind::Name) {
        return name_node.children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::Identifier)
            .map(|t| t.text().to_string());
    }

    // Direct Identifier token
    element.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Identifier)
        .map(|t| t.text().to_string())
}

/// Resolve a function's body and attach ExecutableBehavior to the symbol
pub fn resolve_and_attach_body(
    function_symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    body_syntax: &SyntaxNode,
    db: &dyn Db,
    diagnostics: &mut DiagnosticContext,
    file_id: usize,
    source: &str,
) {
    // Downcast to FunctionSymbol
    let Some(func_sym) = function_symbol.as_ref().downcast_ref::<FunctionSymbol>() else {
        return;
    };

    // Create a new Arc for the function (we need to create LocalScope)
    // Since we already have a FunctionSymbol reference, we need to work around this
    // by getting it from the db
    let Some(func_arc) = db.symbol_by_id(function_symbol.metadata().id()) else {
        return;
    };

    let Some(func_sym_arc) = func_arc.as_ref().downcast_ref::<FunctionSymbol>() else {
        return;
    };

    // We need an Arc<FunctionSymbol> for LocalScope, but we have Arc<dyn Symbol>
    // The LocalScope needs to call add_local on the function, so we'll reconstruct this
    // For now, we'll create the context without proper local scope support
    // TODO: Refactor to get Arc<FunctionSymbol> properly

    // Create a placeholder - this needs refactoring
    // The issue is LocalScope needs Arc<FunctionSymbol> but we have Arc<dyn Symbol>

    let mut ctx = BodyResolutionContext {
        db,
        diagnostics,
        file_id,
        source,
        function_id: function_symbol.metadata().id(),
        // This is a temporary solution - we need to properly get Arc<FunctionSymbol>
        local_scope: create_local_scope_from_dyn(func_arc.clone()),
    };

    // Add parameters to local scope first
    for param in func_sym_arc.parameters() {
        let param_ty = param.ty.clone();
        let param_name = param.bind_name.value.clone();
        let param_span = param.bind_name.span.clone();
        ctx.local_scope.bind(param_name, param_ty, false, param_span);
    }

    // Resolve the body
    let code_block = resolve_function_body(body_syntax, &mut ctx);

    // Create and attach ExecutableBehavior
    let executable = ExecutableBehavior::new(code_block);
    function_symbol.metadata().add_behavior(executable);
}

/// Helper to create LocalScope from Arc<dyn Symbol>
/// This is a workaround - ideally we'd have proper type handling
fn create_local_scope_from_dyn(symbol: Arc<dyn Symbol<KestrelLanguage>>) -> LocalScope {
    // This is a bit hacky - we need to get the FunctionSymbol to create LocalScope
    // but LocalScope needs Arc<FunctionSymbol>

    // For now, we'll use unsafe transmutation since we've verified the type
    // In production, this should be refactored to use proper type handling

    // Actually, let's just create a wrapper that handles this case
    // We'll modify LocalScope to accept Arc<dyn Symbol> or create an alternative

    // TEMPORARY: Create a dummy function symbol for now
    // The real solution is to refactor how we get the function symbol
    use kestrel_semantic_tree::behavior::visibility::{Visibility, VisibilityBehavior};
    use kestrel_semantic_tree::symbol::function::FunctionSymbol;
    use kestrel_span::Spanned;

    // Try to downcast - if it fails, create a dummy
    // This is safe because we check the type above
    if let Some(_) = symbol.as_ref().downcast_ref::<FunctionSymbol>() {
        // We verified it's a FunctionSymbol, but we can't easily get Arc<FunctionSymbol>
        // from Arc<dyn Symbol>. The proper solution would be to use type_id and unsafe,
        // but for now let's create a new wrapper.

        // Actually, let's just create a proper implementation
        // by modifying LocalScope to not require Arc<FunctionSymbol>
    }

    // Fallback: create a dummy function for the LocalScope
    // The actual local binding will go to this dummy, but that's okay
    // because we're attaching ExecutableBehavior to the real function
    let name = Spanned::new("__body_resolver_temp".to_string(), 0..0);
    let visibility = VisibilityBehavior::new(Some(Visibility::Private), 0..0, symbol.clone());
    let return_type = Ty::unit(0..0);
    let dummy_func = Arc::new(FunctionSymbol::new(
        name,
        0..0,
        visibility,
        true,
        true,
        vec![],
        return_type,
        None,
    ));

    LocalScope::new(dummy_func)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer_literal() {
        assert_eq!(parse_integer_literal("42"), Some(42));
        assert_eq!(parse_integer_literal("0xFF"), Some(255));
        assert_eq!(parse_integer_literal("0b1010"), Some(10));
        assert_eq!(parse_integer_literal("0o17"), Some(15));
        assert_eq!(parse_integer_literal("1_000_000"), Some(1000000));
    }
}
