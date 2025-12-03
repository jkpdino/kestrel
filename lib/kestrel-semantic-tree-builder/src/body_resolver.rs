//! Function body resolution.
//!
//! This module handles resolving function bodies from syntax to semantic
//! representations (Expression, Statement, CodeBlock). It runs during the
//! bind phase after all symbols have been created.

use std::sync::Arc;

use kestrel_reporting::{DiagnosticContext, IntoDiagnostic};
use kestrel_semantic_tree::behavior::executable::{CodeBlock, ExecutableBehavior};
use kestrel_semantic_tree::behavior::member_access::MemberAccessBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::behavior::callable::CallableBehavior;
use kestrel_semantic_tree::expr::{CallArgument, Expression, ExprKind, PrimitiveMethod};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::pattern::{Mutability, Pattern};
use kestrel_semantic_tree::stmt::Statement;
use kestrel_semantic_tree::symbol::field::FieldSymbol;
use kestrel_semantic_tree::symbol::function::FunctionSymbol;
use kestrel_semantic_tree::symbol::initializer::InitializerSymbol;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::diagnostics::{
    CannotAccessMemberOnTypeError, ExplicitInitSuppressesImplicitError,
    FieldNotVisibleForInitError, ImplicitInitArityError, ImplicitInitLabelError,
    InstanceMethodOnTypeError, MemberNotAccessibleError, MemberNotVisibleError,
    NoMatchingInitializerError, NoMatchingMethodError, NoMatchingOverloadError,
    NoSuchMemberError, NoSuchMethodError, OverloadDescription,
    PrimitiveMethodNotCallableError, SelfOutsideInstanceMethodError, UndefinedNameError,
};
use crate::local_scope::LocalScope;
use crate::path_resolver::is_visible_from;
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
        | SyntaxKind::ExprCall
        | SyntaxKind::ExprAssignment
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

        SyntaxKind::ExprCall => {
            resolve_call_expression(expr_node, ctx)
        }

        SyntaxKind::ExprAssignment => {
            resolve_assignment_expression(expr_node, ctx)
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

/// Resolve an assignment expression: target = value
fn resolve_assignment_expression(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let span = get_node_span(node, ctx.source);

    // Find the LHS and RHS expressions
    // ExprAssignment contains: Expression, Equals token, Expression
    let mut expr_children = node.children()
        .filter(|c| c.kind() == SyntaxKind::Expression || is_expression_kind(c.kind()));

    let lhs_node = match expr_children.next() {
        Some(n) => n,
        None => return Expression::error(span),
    };

    let rhs_node = match expr_children.next() {
        Some(n) => n,
        None => return Expression::error(span),
    };

    // Resolve both sides
    let target = resolve_expression(&lhs_node, ctx);
    let value = resolve_expression(&rhs_node, ctx);

    // TODO: Validate that target is assignable (var, not let; field on mutable receiver)
    // TODO: Type check that value type is compatible with target type

    Expression::assignment(target, value, span)
}

/// Resolve a call expression: callee(arg1, arg2, ...)
fn resolve_call_expression(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let span = get_node_span(node, ctx.source);

    // Find the callee expression (first child that's an Expression)
    let callee_node = match node.children().find(|c| c.kind() == SyntaxKind::Expression || is_expression_kind(c.kind())) {
        Some(n) => n,
        None => return Expression::error(span.clone()),
    };

    // Find the argument list
    let arg_list_node = node.children().find(|c| c.kind() == SyntaxKind::ArgumentList);

    // Resolve callee first
    let callee = resolve_expression(&callee_node, ctx);

    // Parse arguments
    let arguments = if let Some(arg_list) = arg_list_node {
        resolve_argument_list(&arg_list, ctx)
    } else {
        vec![]
    };

    // Get labels for overload resolution (owned strings)
    let arg_labels: Vec<Option<String>> = arguments.iter()
        .map(|a| a.label.clone())
        .collect();

    // Now resolve based on callee type
    resolve_call(callee, arguments, &arg_labels, span, ctx)
}

/// Resolve an argument list node into CallArguments
fn resolve_argument_list(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Vec<CallArgument> {
    let mut arguments = Vec::new();

    for child in node.children() {
        if child.kind() == SyntaxKind::Argument {
            if let Some(arg) = resolve_argument(&child, ctx) {
                arguments.push(arg);
            }
        }
    }

    arguments
}

/// Resolve a single argument node
fn resolve_argument(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Option<CallArgument> {
    let span = get_node_span(node, ctx.source);

    // Check for label (Identifier followed by Colon)
    let mut label = None;
    let mut has_colon = false;

    for elem in node.children_with_tokens() {
        if let Some(token) = elem.as_token() {
            if token.kind() == SyntaxKind::Identifier && !has_colon {
                // This might be a label
                label = Some(token.text().to_string());
            } else if token.kind() == SyntaxKind::Colon {
                has_colon = true;
            }
        }
    }

    // If we found a colon, the identifier was a label; otherwise it wasn't
    if !has_colon {
        label = None;
    }

    // Find the value expression
    let value_node = node.children()
        .find(|c| c.kind() == SyntaxKind::Expression || is_expression_kind(c.kind()))?;

    let value = resolve_expression(&value_node, ctx);

    Some(CallArgument::unlabeled(value, span))
        .map(|mut arg| {
            if let Some(l) = label {
                arg.label = Some(l);
            }
            arg
        })
}

/// Resolve a call with the given callee and arguments
fn resolve_call(
    callee: Expression,
    arguments: Vec<CallArgument>,
    arg_labels: &[Option<String>],
    span: Span,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    // Clone callee.kind to avoid borrow issues
    let callee_kind = callee.kind.clone();
    let callee_ty = callee.ty.clone();

    match callee_kind {
        // Direct function reference
        ExprKind::SymbolRef(symbol_id) => {
            resolve_single_function_call(symbol_id, callee, arguments, span, ctx)
        }

        // Overloaded function reference - need to pick one
        ExprKind::OverloadedRef(ref candidates) => {
            resolve_overloaded_call(candidates, callee, arguments, arg_labels, span, ctx)
        }

        // Method reference (from member access on a type)
        ExprKind::MethodRef { ref receiver, ref candidates, ref method_name } => {
            resolve_method_call(receiver, candidates, method_name, arguments, arg_labels, span, ctx)
        }

        // Field access - might be method call on struct
        ExprKind::FieldAccess { ref object, ref field } => {
            // This could be:
            // 1. A field with callable type (first-class function)
            // 2. A method call
            resolve_member_call(object, field, arguments, arg_labels, span, ctx)
        }

        // Type reference - struct instantiation
        ExprKind::TypeRef(symbol_id) => {
            resolve_struct_instantiation(symbol_id, arguments, arg_labels, span, ctx)
        }

        // Local variable reference - could be calling a function stored in a variable
        ExprKind::LocalRef(_local_id) => {
            // Check if the type is callable
            if let TyKind::Function { return_type, .. } = callee_ty.kind() {
                Expression::call(callee, arguments, (**return_type).clone(), span)
            } else {
                // TODO: Report error: trying to call non-callable
                Expression::error(span)
            }
        }

        // Any other expression - check if callable type
        _ => {
            if let TyKind::Function { return_type, .. } = callee_ty.kind() {
                Expression::call(callee, arguments, (**return_type).clone(), span)
            } else {
                // TODO: Report error: expression is not callable
                Expression::error(span)
            }
        }
    }
}

/// Resolve a call to a single function (not overloaded)
fn resolve_single_function_call(
    symbol_id: SymbolId,
    callee: Expression,
    arguments: Vec<CallArgument>,
    span: Span,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    // Get the function symbol
    let Some(symbol) = ctx.db.symbol_by_id(symbol_id) else {
        return Expression::error(span);
    };

    // Get the callable behavior
    let Some(callable) = get_callable_behavior(&symbol) else {
        return Expression::error(span);
    };

    // Check if this is an instance method being called without an instance
    // This happens when we have Type.instanceMethod() instead of instance.instanceMethod()
    if callable.is_instance_method() {
        // Get the parent type name for the error message
        let type_name = symbol
            .metadata()
            .parent()
            .map(|p| p.metadata().name().value.clone())
            .unwrap_or_else(|| "<unknown>".to_string());
        let method_name = symbol.metadata().name().value.clone();

        let error = InstanceMethodOnTypeError {
            span: span.clone(),
            type_name,
            method_name,
        };
        ctx.diagnostics
            .add_diagnostic(error.into_diagnostic(ctx.file_id));
        return Expression::error(span);
    }

    // Check arity and labels
    let arg_labels: Vec<Option<String>> = arguments.iter()
        .map(|a| a.label.clone())
        .collect();

    if !matches_signature(&callable, arguments.len(), &arg_labels) {
        // Report error - wrong arity or labels
        let function_name = symbol.metadata().name().value.clone();
        let available_overloads = vec![collect_single_overload_description(&symbol)];

        let error = NoMatchingOverloadError {
            call_span: span.clone(),
            name: function_name,
            provided_labels: arg_labels,
            provided_arity: arguments.len(),
            available_overloads,
        };
        ctx.diagnostics
            .add_diagnostic(error.into_diagnostic(ctx.file_id));
        return Expression::error(span);
    }

    let return_ty = callable.return_type().clone();
    Expression::call(callee, arguments, return_ty, span)
}

/// Collect a single overload description from a symbol.
fn collect_single_overload_description(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> OverloadDescription {
    let name = symbol.metadata().name().value.clone();
    let callable = get_callable_behavior(symbol);

    match callable {
        Some(cb) => {
            let labels: Vec<Option<String>> = cb
                .parameters()
                .iter()
                .map(|p| p.external_label().map(|s| s.to_string()))
                .collect();
            let param_types: Vec<String> = cb
                .parameters()
                .iter()
                .map(|p| format_type(&p.ty))
                .collect();

            OverloadDescription {
                name,
                labels,
                param_types,
                definition_span: Some(symbol.metadata().name().span.clone()),
                definition_file_id: None,
            }
        }
        None => OverloadDescription {
            name,
            labels: vec![],
            param_types: vec![],
            definition_span: Some(symbol.metadata().name().span.clone()),
            definition_file_id: None,
        },
    }
}

/// Resolve an overloaded function call by matching arity + labels
fn resolve_overloaded_call(
    candidates: &[SymbolId],
    callee: Expression,
    arguments: Vec<CallArgument>,
    arg_labels: &[Option<String>],
    span: Span,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    // Find the matching overload
    for &candidate_id in candidates {
        if let Some(symbol) = ctx.db.symbol_by_id(candidate_id) {
            if let Some(callable) = get_callable_behavior(&symbol) {
                if matches_signature(&callable, arguments.len(), arg_labels) {
                    let return_ty = callable.return_type().clone();
                    let resolved_callee = Expression::symbol_ref(candidate_id, callee.ty.clone(), callee.span.clone());
                    return Expression::call(resolved_callee, arguments, return_ty, span);
                }
            }
        }
    }

    // No match found - collect overload info for error message
    let function_name = get_function_name_from_candidates(candidates, ctx.db);
    let available_overloads = collect_overload_descriptions(candidates, ctx.db);

    let error = NoMatchingOverloadError {
        call_span: span.clone(),
        name: function_name,
        provided_labels: arg_labels.to_vec(),
        provided_arity: arguments.len(),
        available_overloads,
    };
    ctx.diagnostics
        .add_diagnostic(error.into_diagnostic(ctx.file_id));

    Expression::error(span)
}

/// Resolve struct instantiation: `StructName(x: 1, y: 2)`
///
/// This handles both explicit initializers and implicit memberwise initialization.
fn resolve_struct_instantiation(
    symbol_id: SymbolId,
    arguments: Vec<CallArgument>,
    arg_labels: &[Option<String>],
    span: Span,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    // Get the struct symbol
    let Some(symbol) = ctx.db.symbol_by_id(symbol_id) else {
        return Expression::error(span);
    };

    // Verify it's a struct
    if symbol.metadata().kind() != KestrelSymbolKind::Struct {
        // Not a struct - cannot instantiate
        // TODO: Add proper error diagnostic
        return Expression::error(span);
    }

    let Some(struct_sym) = symbol.as_ref().downcast_ref::<StructSymbol>() else {
        return Expression::error(span);
    };

    // Check for explicit initializers
    let explicit_inits: Vec<Arc<dyn Symbol<KestrelLanguage>>> = symbol
        .metadata()
        .children()
        .into_iter()
        .filter(|c| c.metadata().kind() == KestrelSymbolKind::Initializer)
        .collect();

    if !explicit_inits.is_empty() {
        // Has explicit initializers - find matching one
        return resolve_explicit_init_call(&explicit_inits, arguments, arg_labels, span, symbol.clone(), ctx);
    }

    // No explicit initializers - try implicit memberwise init
    resolve_implicit_init(symbol_id, arguments, arg_labels, span, symbol.clone(), ctx)
}

/// Resolve a call to an explicit initializer
fn resolve_explicit_init_call(
    initializers: &[Arc<dyn Symbol<KestrelLanguage>>],
    arguments: Vec<CallArgument>,
    arg_labels: &[Option<String>],
    span: Span,
    struct_symbol: Arc<dyn Symbol<KestrelLanguage>>,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    // Find matching initializer by arity and labels
    for init_sym in initializers {
        if let Some(callable) = get_callable_behavior(init_sym) {
            if matches_signature(&callable, arguments.len(), arg_labels) {
                // Found matching initializer
                // The return type is Self (the struct type)
                let struct_ty = Ty::self_type(span.clone());

                // For explicit init, create a Call expression
                let init_id = init_sym.metadata().id();
                let init_ref = Expression::symbol_ref(init_id, Ty::inferred(span.clone()), span.clone());
                return Expression::call(init_ref, arguments, struct_ty, span);
            }
        }
    }

    // No matching initializer found - report error
    let struct_name = struct_symbol.metadata().name().value.clone();

    // Build list of available initializers for the error message
    let available_initializers: Vec<OverloadDescription> = initializers
        .iter()
        .filter_map(|init| {
            let callable = get_callable_behavior(init)?;
            let labels: Vec<Option<String>> = callable
                .parameters()
                .iter()
                .map(|p| p.label.as_ref().map(|l| l.value.clone()))
                .collect();
            let param_types: Vec<String> = callable
                .parameters()
                .iter()
                .map(|p| format_type(&p.ty))
                .collect();
            Some(OverloadDescription {
                name: struct_name.clone(),
                labels,
                param_types,
                definition_span: Some(init.metadata().span().clone()),
                definition_file_id: Some(ctx.file_id),
            })
        })
        .collect();

    let error = NoMatchingInitializerError {
        span: span.clone(),
        struct_name,
        provided_labels: arg_labels.to_vec(),
        provided_arity: arguments.len(),
        available_initializers,
    };
    ctx.diagnostics.add_diagnostic(error.into_diagnostic(ctx.file_id));

    Expression::error(span)
}

/// Resolve implicit memberwise initialization
///
/// The struct must not have any explicit initializers and all fields must be visible.
fn resolve_implicit_init(
    _struct_symbol_id: SymbolId,
    arguments: Vec<CallArgument>,
    arg_labels: &[Option<String>],
    span: Span,
    struct_symbol: Arc<dyn Symbol<KestrelLanguage>>,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let struct_name = struct_symbol.metadata().name().value.clone();

    // Collect fields in declaration order
    let fields: Vec<Arc<dyn Symbol<KestrelLanguage>>> = struct_symbol
        .metadata()
        .children()
        .into_iter()
        .filter(|c| c.metadata().kind() == KestrelSymbolKind::Field)
        .collect();

    let field_names: Vec<String> = fields
        .iter()
        .map(|f| f.metadata().name().value.clone())
        .collect();

    // Check visibility of all fields
    let context_sym = ctx.db.symbol_by_id(ctx.function_id);
    for field in &fields {
        if let Some(ref ctx_sym) = context_sym {
            if !is_visible_from(field, ctx_sym) {
                // Field is not visible - cannot use implicit init
                let error = FieldNotVisibleForInitError {
                    span: span.clone(),
                    struct_name: struct_name.clone(),
                    field_name: field.metadata().name().value.clone(),
                    field_visibility: "private".to_string(), // TODO: Get actual visibility
                };
                ctx.diagnostics.add_diagnostic(error.into_diagnostic(ctx.file_id));
                return Expression::error(span);
            }
        }
    }

    // Validate arguments match fields in order
    if arguments.len() != fields.len() {
        let error = ImplicitInitArityError {
            span: span.clone(),
            struct_name,
            expected: fields.len(),
            provided: arguments.len(),
            field_names,
        };
        ctx.diagnostics.add_diagnostic(error.into_diagnostic(ctx.file_id));
        return Expression::error(span);
    }

    // Check that labels match field names
    for (i, (field, _arg)) in fields.iter().zip(arguments.iter()).enumerate() {
        let field_name = field.metadata().name().value.clone();
        let expected_label = Some(field_name.clone());
        let provided_label = arg_labels.get(i).cloned().flatten();

        if arg_labels.get(i) != Some(&expected_label) {
            let error = ImplicitInitLabelError {
                span: span.clone(),
                struct_name: struct_name.clone(),
                arg_index: i,
                provided_label,
                expected_label: field_name,
            };
            ctx.diagnostics.add_diagnostic(error.into_diagnostic(ctx.file_id));
            return Expression::error(span);
        }
    }

    // All checks passed - create ImplicitStructInit expression
    // Use Ty::self_type for now - actual struct type resolution happens elsewhere
    let struct_ty = Ty::self_type(span.clone());

    Expression::implicit_struct_init(struct_ty, arguments, span)
}

/// Resolve a method call from a MethodRef expression
fn resolve_method_call(
    receiver: &Expression,
    candidates: &[SymbolId],
    method_name: &str,
    arguments: Vec<CallArgument>,
    arg_labels: &[Option<String>],
    span: Span,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    // Find matching overload
    for &candidate_id in candidates {
        if let Some(symbol) = ctx.db.symbol_by_id(candidate_id) {
            if let Some(callable) = get_callable_behavior(&symbol) {
                if matches_signature(&callable, arguments.len(), arg_labels) {
                    // Check visibility
                    if let Some(context_sym) = ctx.db.symbol_by_id(ctx.function_id) {
                        if !is_visible_from(&symbol, &context_sym) {
                            // TODO: Report error: method not visible
                            continue;
                        }
                    }

                    let return_ty = callable.return_type().clone();

                    // Create method ref and then call
                    let method_ref = Expression::method_ref(
                        receiver.clone(),
                        vec![candidate_id],
                        method_name.to_string(),
                        span.clone(),
                    );

                    return Expression::call(method_ref, arguments, return_ty, span);
                }
            }
        }
    }

    // No matching method found - collect overload info for error message
    let receiver_type = format_type(&receiver.ty);
    let available_overloads = collect_overload_descriptions(candidates, ctx.db);

    let error = NoMatchingMethodError {
        call_span: span.clone(),
        method_name: method_name.to_string(),
        receiver_type,
        provided_labels: arg_labels.to_vec(),
        provided_arity: arguments.len(),
        available_overloads,
    };
    ctx.diagnostics
        .add_diagnostic(error.into_diagnostic(ctx.file_id));

    Expression::error(span)
}

/// Resolve a member call from a FieldAccess expression: obj.method(args)
fn resolve_member_call(
    object: &Expression,
    member_name: &str,
    arguments: Vec<CallArgument>,
    arg_labels: &[Option<String>],
    span: Span,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let base_ty = &object.ty;

    // First check for primitive method
    if let Some(primitive_method) = PrimitiveMethod::lookup(base_ty, member_name) {
        return Expression::primitive_method_call(
            object.clone(),
            primitive_method,
            arguments,
            span,
        );
    }

    // Get container from type
    let container = match get_type_container(base_ty, ctx) {
        Some(c) => c,
        None => {
            // Report error: cannot call method on this type
            let error = NoSuchMethodError {
                call_span: span.clone(),
                method_name: member_name.to_string(),
                receiver_type: format_type(base_ty),
            };
            ctx.diagnostics
                .add_diagnostic(error.into_diagnostic(ctx.file_id));
            return Expression::error(span);
        }
    };

    // Find method(s) with this name
    let methods: Vec<Arc<dyn Symbol<KestrelLanguage>>> = container
        .metadata()
        .children()
        .into_iter()
        .filter(|c| {
            c.metadata().kind() == KestrelSymbolKind::Function
                && c.metadata().name().value == member_name
        })
        .collect();

    if methods.is_empty() {
        // Report error: no such method
        let error = NoSuchMethodError {
            call_span: span.clone(),
            method_name: member_name.to_string(),
            receiver_type: format_type(base_ty),
        };
        ctx.diagnostics
            .add_diagnostic(error.into_diagnostic(ctx.file_id));
        return Expression::error(span);
    }

    // Find matching overload
    for method in &methods {
        if let Some(callable) = get_callable_behavior(method) {
            if matches_signature(&callable, arguments.len(), arg_labels) {
                // Check visibility
                if let Some(context_sym) = ctx.db.symbol_by_id(ctx.function_id) {
                    if !is_visible_from(method, &context_sym) {
                        // TODO: Report error: method not visible
                        continue;
                    }
                }

                let return_ty = callable.return_type().clone();
                let method_id = method.metadata().id();

                // Create method ref and then call
                let method_ref = Expression::method_ref(
                    object.clone(),
                    vec![method_id],
                    member_name.to_string(),
                    span.clone(),
                );

                return Expression::call(method_ref, arguments, return_ty, span);
            }
        }
    }

    // No matching method found - collect overload info for error message
    let receiver_type = format_type(base_ty);
    let method_ids: Vec<SymbolId> = methods.iter().map(|m| m.metadata().id()).collect();
    let available_overloads = collect_overload_descriptions(&method_ids, ctx.db);

    let error = NoMatchingMethodError {
        call_span: span.clone(),
        method_name: member_name.to_string(),
        receiver_type,
        provided_labels: arg_labels.to_vec(),
        provided_arity: arguments.len(),
        available_overloads,
    };
    ctx.diagnostics
        .add_diagnostic(error.into_diagnostic(ctx.file_id));

    Expression::error(span)
}

/// Check if a callable signature matches the given arity and labels
fn matches_signature(callable: &CallableBehavior, arity: usize, labels: &[Option<String>]) -> bool {
    let params = callable.parameters();

    // Check arity
    if params.len() != arity {
        return false;
    }

    // Check labels match
    for (param, label) in params.iter().zip(labels.iter()) {
        let param_label = param.external_label();
        let label_ref = label.as_deref();
        if param_label != label_ref {
            return false;
        }
    }

    true
}

/// Get the CallableBehavior from a symbol if it has one
fn get_callable_behavior(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<CallableBehavior> {
    for behavior in symbol.metadata().behaviors() {
        if behavior.kind() == KestrelBehaviorKind::Callable {
            if let Some(callable) = behavior.as_ref().downcast_ref::<CallableBehavior>() {
                return Some(callable.clone());
            }
        }
    }
    None
}

/// Get the return type from a callable symbol
fn get_callable_return_type(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<Ty> {
    get_callable_behavior(symbol).map(|c| c.return_type().clone())
}

/// Resolve a path expression (variable reference, function reference, or member access)
fn resolve_path_expression(
    node: &SyntaxNode,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let span = get_node_span(node, ctx.source);

    // Check for nested expression inside the path (happens with member access on call expressions)
    // e.g., `obj.method().field` is parsed as ExprPath containing ExprCall
    if let Some(nested_expr) = find_nested_expression(node) {
        let base = resolve_expression(&nested_expr, ctx);
        let trailing_members = extract_trailing_identifiers(node, ctx.source);
        if trailing_members.is_empty() {
            return base;
        }
        return resolve_member_chain(base, &trailing_members, ctx);
    }

    // Extract the path segments with their spans
    let path_with_spans = extract_path_segments_with_spans(node, ctx.source);

    if path_with_spans.is_empty() {
        return Expression::error(span);
    }

    // Extract just the names for lookups
    let path: Vec<String> = path_with_spans.iter().map(|(name, _)| name.clone()).collect();
    let first_name = &path[0];
    let first_span = path_with_spans[0].1.clone();

    // First, check if it's a local variable
    if let Some(local_id) = ctx.local_scope.lookup(first_name) {
        // Get the type from the local
        let local_ty = ctx.local_scope.function()
            .get_local(local_id)
            .map(|l| l.ty().clone())
            .unwrap_or_else(|| Ty::error(span.clone()));

        let base_expr = Expression::local_ref(local_id, local_ty, first_span);

        // If there are more segments, they are member accesses
        if path_with_spans.len() == 1 {
            return base_expr;
        } else {
            return resolve_member_chain(base_expr, &path_with_spans[1..], ctx);
        }
    }

    // Check if this is 'self' being used outside an instance method
    if first_name == "self" {
        // 'self' was not found in local scope, which means we're not in an instance method
        let context = get_function_context(ctx);
        let error = SelfOutsideInstanceMethodError {
            span: first_span.clone(),
            context,
        };
        ctx.diagnostics
            .add_diagnostic(error.into_diagnostic(ctx.file_id));
        return Expression::error(span);
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
            // Report undefined name error
            let error_span = if index < path_with_spans.len() {
                path_with_spans[index].1.clone()
            } else {
                first_span.clone()
            };
            let error = UndefinedNameError {
                span: error_span,
                name: segment,
            };
            ctx.diagnostics
                .add_diagnostic(error.into_diagnostic(ctx.file_id));
            Expression::error(span)
        }
        ValuePathResolution::Ambiguous { segment, index, candidates } => {
            // TODO: Report ambiguous name diagnostic
            let _ = (segment, index, candidates);
            Expression::error(span)
        }
        ValuePathResolution::NotAValue { symbol_id } => {
            // This is a type reference (e.g., struct name) - may be used for initialization
            // The actual type resolution happens during call resolution
            Expression::type_ref(symbol_id, Ty::inferred(span.clone()), span)
        }
    }
}

/// Get a description of the function context for error messages.
///
/// Returns descriptions like "static method", "free function", etc.
fn get_function_context(ctx: &BodyResolutionContext) -> String {
    let Some(function) = ctx.db.symbol_by_id(ctx.function_id) else {
        return "this context".to_string();
    };

    // Check if the function is in a struct or protocol
    let parent = function.metadata().parent();
    match parent.as_ref().map(|p| p.metadata().kind()) {
        Some(KestrelSymbolKind::Struct) | Some(KestrelSymbolKind::Protocol) => {
            // It's a method - check if static
            // We can check by looking for 'self' in local scope, but we already know
            // 'self' wasn't found, so this must be a static method
            "static method".to_string()
        }
        _ => {
            // Not in a struct/protocol, so it's a free function
            "free function".to_string()
        }
    }
}

/// Resolve a chain of member accesses: obj.field1.field2.field3
fn resolve_member_chain(
    base: Expression,
    members: &[(String, Span)],
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let mut current = base;

    for (member_name, member_span) in members {
        current = resolve_member_access(current, member_name, member_span.clone(), ctx);
    }

    current
}

/// Resolve a single member access: base.member
///
/// This function:
/// 1. Checks for primitive methods on primitive types
/// 2. Gets the container type from the base expression
/// 3. Finds a child symbol with the given name
/// 4. Checks visibility
/// 5. Uses MemberAccessBehavior to produce the result expression
fn resolve_member_access(
    base: Expression,
    member_name: &str,
    member_span: Span,
    ctx: &mut BodyResolutionContext,
) -> Expression {
    let base_span = base.span.clone();
    let base_ty = &base.ty;
    let full_span = base_span.start..member_span.end;

    // 1. Check for primitive method (e.g., 5.toString, "hello".length)
    // Primitive methods can only be called, not used as first-class values
    if let Some(primitive_method) = PrimitiveMethod::lookup(base_ty, member_name) {
        // Primitive methods cannot be used as first-class values.
        // Report an error - they must be called directly.
        let error = PrimitiveMethodNotCallableError {
            span: full_span.clone(),
            method_name: primitive_method.name().to_string(),
            receiver_type: format_type(base_ty),
        };
        ctx.diagnostics
            .add_diagnostic(error.into_diagnostic(ctx.file_id));
        return Expression::error(full_span);
    }

    // 2. Get container from base type
    let container = match get_type_container(base_ty, ctx) {
        Some(c) => c,
        None => {
            // Type doesn't support member access (e.g., Int, Bool, etc.)
            let error = CannotAccessMemberOnTypeError {
                span: full_span.clone(),
                base_type: format_type(base_ty),
            };
            ctx.diagnostics
                .add_diagnostic(error.into_diagnostic(ctx.file_id));
            return Expression::error(full_span);
        }
    };

    // 2. Find child with that name
    let member = container
        .metadata()
        .children()
        .into_iter()
        .find(|c| c.metadata().name().value == member_name);

    let member = match member {
        Some(m) => m,
        None => {
            let error = NoSuchMemberError {
                member_span,
                member_name: member_name.to_string(),
                base_span,
                base_type: format_type(base_ty),
            };
            ctx.diagnostics
                .add_diagnostic(error.into_diagnostic(ctx.file_id));
            return Expression::error(full_span);
        }
    };

    // 3. Check visibility
    let context_symbol = ctx.db.symbol_by_id(ctx.function_id);
    if let Some(ref context_sym) = context_symbol {
        if !is_visible_from(&member, context_sym) {
            use kestrel_semantic_tree::behavior::visibility::Visibility;
            use kestrel_semantic_tree::behavior_ext::SymbolBehaviorExt;

            let visibility = member
                .visibility_behavior()
                .and_then(|v| v.visibility().cloned())
                .unwrap_or(Visibility::Internal);

            let error = MemberNotVisibleError {
                member_span,
                member_name: member_name.to_string(),
                base_span,
                base_type: format_type(base_ty),
                visibility: visibility.to_string(),
            };
            ctx.diagnostics
                .add_diagnostic(error.into_diagnostic(ctx.file_id));
            return Expression::error(full_span);
        }
    }

    // 4. Get MemberAccessBehavior and produce expression
    for behavior in member.metadata().behaviors() {
        if behavior.kind() == KestrelBehaviorKind::MemberAccess {
            if let Some(access) = behavior.as_ref().downcast_ref::<MemberAccessBehavior>() {
                return access.access(base, full_span);
            }
        }
    }

    // 5. If it's a function, create a MethodRef (for method calls like obj.method())
    if member.metadata().kind() == KestrelSymbolKind::Function {
        // Find all methods with this name (for overloads)
        let candidates: Vec<SymbolId> = container
            .metadata()
            .children()
            .into_iter()
            .filter(|c| {
                c.metadata().kind() == KestrelSymbolKind::Function
                    && c.metadata().name().value == member_name
            })
            .map(|c| c.metadata().id())
            .collect();

        return Expression::method_ref(base, candidates, member_name.to_string(), full_span);
    }

    // Member exists but doesn't have MemberAccessBehavior (e.g., type alias, nested type)
    let error = MemberNotAccessibleError {
        member_span,
        member_name: member_name.to_string(),
        base_span,
        base_type: format_type(base_ty),
        member_kind: format_symbol_kind(member.metadata().kind()),
    };
    ctx.diagnostics
        .add_diagnostic(error.into_diagnostic(ctx.file_id));
    Expression::error(full_span)
}

/// Get the container symbol from a type (for member lookup)
fn get_type_container(ty: &Ty, ctx: &BodyResolutionContext) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
    match ty.kind() {
        TyKind::Struct { symbol, .. } => Some(symbol.clone() as Arc<dyn Symbol<KestrelLanguage>>),
        TyKind::Protocol { symbol, .. } => Some(symbol.clone() as Arc<dyn Symbol<KestrelLanguage>>),
        TyKind::SelfType => {
            // Resolve Self to the containing struct/protocol
            // Get the function symbol, then its parent (which should be the struct/protocol)
            let function = ctx.db.symbol_by_id(ctx.function_id)?;
            let parent = function.metadata().parent()?;
            match parent.metadata().kind() {
                KestrelSymbolKind::Struct | KestrelSymbolKind::Protocol => Some(parent),
                _ => None,
            }
        }
        // TODO: Handle other types that can have members
        _ => None,
    }
}

/// Format a type for error messages
fn format_type(ty: &Ty) -> String {
    match ty.kind() {
        TyKind::Unit => "()".to_string(),
        TyKind::Never => "!".to_string(),
        TyKind::Bool => "Bool".to_string(),
        TyKind::String => "String".to_string(),
        TyKind::Int(bits) => format!("{:?}", bits),
        TyKind::Float(bits) => format!("{:?}", bits),
        TyKind::Tuple(elements) => {
            let items: Vec<_> = elements.iter().map(format_type).collect();
            format!("({})", items.join(", "))
        }
        TyKind::Array(elem) => format!("[{}]", format_type(elem)),
        TyKind::Function { params, return_type } => {
            let params_str: Vec<_> = params.iter().map(format_type).collect();
            format!("({}) -> {}", params_str.join(", "), format_type(return_type))
        }
        TyKind::Struct { symbol, .. } => symbol.metadata().name().value.clone(),
        TyKind::Protocol { symbol, .. } => symbol.metadata().name().value.clone(),
        TyKind::TypeParameter(param) => param.metadata().name().value.clone(),
        TyKind::TypeAlias { symbol, .. } => symbol.metadata().name().value.clone(),
        TyKind::SelfType => "Self".to_string(),
        TyKind::Inferred => "_".to_string(),
        TyKind::Error => "<error>".to_string(),
    }
}

/// Format a symbol kind for error messages
fn format_symbol_kind(kind: KestrelSymbolKind) -> String {
    match kind {
        KestrelSymbolKind::Field => "field".to_string(),
        KestrelSymbolKind::Function => "function".to_string(),
        KestrelSymbolKind::Import => "import".to_string(),
        KestrelSymbolKind::Initializer => "initializer".to_string(),
        KestrelSymbolKind::Module => "module".to_string(),
        KestrelSymbolKind::Protocol => "protocol".to_string(),
        KestrelSymbolKind::SourceFile => "source file".to_string(),
        KestrelSymbolKind::Struct => "struct".to_string(),
        KestrelSymbolKind::TypeAlias => "type alias".to_string(),
        KestrelSymbolKind::TypeParameter => "type parameter".to_string(),
    }
}

/// Extract path segments with their spans from a path expression node
fn extract_path_segments_with_spans(node: &SyntaxNode, source: &str) -> Vec<(String, Span)> {
    let mut segments = Vec::new();

    // ExprPath may contain Path or direct PathElements
    if let Some(path_node) = node.children().find(|c| c.kind() == SyntaxKind::Path) {
        // Path contains PathElements
        for element in path_node.children() {
            if element.kind() == SyntaxKind::PathElement {
                if let Some((name, span)) = extract_path_element_name_with_span(&element, source) {
                    segments.push((name, span));
                }
            }
        }
    } else {
        // Direct identifiers
        for child in node.children() {
            if child.kind() == SyntaxKind::PathElement {
                if let Some((name, span)) = extract_path_element_name_with_span(&child, source) {
                    segments.push((name, span));
                }
            }
        }

        // Fallback: look for Name or Identifier tokens
        if segments.is_empty() {
            for elem in node.children_with_tokens() {
                if let Some(token) = elem.as_token() {
                    if token.kind() == SyntaxKind::Identifier {
                        let span = token.text_range();
                        segments.push((
                            token.text().to_string(),
                            span.start().into()..span.end().into(),
                        ));
                    }
                }
            }
        }
    }

    segments
}

/// Extract the name and span from a PathElement node
fn extract_path_element_name_with_span(element: &SyntaxNode, _source: &str) -> Option<(String, Span)> {
    // PathElement contains Name or Identifier
    if let Some(name_node) = element.children().find(|c| c.kind() == SyntaxKind::Name) {
        return name_node
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::Identifier)
            .map(|t| {
                let range = t.text_range();
                (t.text().to_string(), range.start().into()..range.end().into())
            });
    }

    // Direct Identifier token
    element
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Identifier)
        .map(|t| {
            let range = t.text_range();
            (t.text().to_string(), range.start().into()..range.end().into())
        })
}

/// Find a nested expression inside a path node
///
/// This handles the case where member access on a call expression is emitted as
/// an ExprPath containing an Expression/ExprCall child.
/// e.g., `obj.method().field` is parsed as ExprPath containing ExprCall
fn find_nested_expression(node: &SyntaxNode) -> Option<SyntaxNode> {
    // We're looking inside an ExprPath node. Normally it contains only identifiers and dots.
    // But when member access is on a call expression, the parser emits the call inside the ExprPath.
    // We need to find such nested call expressions.

    for child in node.children() {
        // Look for Expression wrapper containing a non-path expression
        if child.kind() == SyntaxKind::Expression {
            // Check if this Expression contains an ExprCall or other complex (non-path) expression
            for inner in child.children() {
                // Only return if it's a complex expression type, not just another path
                if inner.kind() == SyntaxKind::ExprCall {
                    return Some(child);
                }
            }
        }
        // Also check for direct ExprCall nodes
        if child.kind() == SyntaxKind::ExprCall {
            return Some(child);
        }
    }
    None
}

/// Extract trailing identifier tokens after a nested expression in a path
///
/// When a path contains a nested expression (e.g., from member access on a call),
/// this extracts the identifiers that appear after the expression.
fn extract_trailing_identifiers(node: &SyntaxNode, _source: &str) -> Vec<(String, Span)> {
    let mut identifiers = Vec::new();
    let mut found_expression = false;

    for elem in node.children_with_tokens() {
        if let Some(child) = elem.as_node() {
            // Mark when we see the nested expression
            if child.kind() == SyntaxKind::Expression || is_expression_kind(child.kind()) {
                found_expression = true;
            }
        } else if let Some(token) = elem.as_token() {
            // Only collect identifiers after the expression
            if found_expression && token.kind() == SyntaxKind::Identifier {
                let range = token.text_range();
                identifiers.push((
                    token.text().to_string(),
                    range.start().into()..range.end().into(),
                ));
            }
        }
    }

    identifiers
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

/// Get the function name from a list of candidate symbol IDs.
fn get_function_name_from_candidates(candidates: &[SymbolId], db: &dyn Db) -> String {
    for &candidate_id in candidates {
        if let Some(symbol) = db.symbol_by_id(candidate_id) {
            return symbol.metadata().name().value.clone();
        }
    }
    "<unknown>".to_string()
}

/// Collect overload descriptions from a list of candidate symbol IDs.
fn collect_overload_descriptions(candidates: &[SymbolId], db: &dyn Db) -> Vec<OverloadDescription> {
    let mut descriptions = Vec::new();

    for &candidate_id in candidates {
        if let Some(symbol) = db.symbol_by_id(candidate_id) {
            if let Some(callable) = get_callable_behavior(&symbol) {
                let name = symbol.metadata().name().value.clone();
                let labels: Vec<Option<String>> = callable
                    .parameters()
                    .iter()
                    .map(|p| p.external_label().map(|s| s.to_string()))
                    .collect();
                let param_types: Vec<String> = callable
                    .parameters()
                    .iter()
                    .map(|p| format_type(&p.ty))
                    .collect();

                descriptions.push(OverloadDescription {
                    name,
                    labels,
                    param_types,
                    definition_span: Some(symbol.metadata().name().span.clone()),
                    definition_file_id: None, // TODO: Get file ID from symbol
                });
            }
        }
    }

    descriptions
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
