//! Call expression resolution.
//!
//! This module handles resolving function calls, method calls, overloaded calls,
//! and struct instantiation (both explicit and implicit initializers).

use std::sync::Arc;

use kestrel_reporting::IntoDiagnostic;
use kestrel_semantic_tree::expr::{CallArgument, Expression, ExprKind};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::diagnostics::{
    FieldNotVisibleForInitError, ImplicitInitArityError, ImplicitInitLabelError,
    InstanceMethodOnTypeError, NoMatchingInitializerError, NoMatchingMethodError,
    NoMatchingOverloadError, OverloadDescription,
};
use crate::path_resolver::is_visible_from;
use crate::queries::Db;
use crate::utils::get_node_span;

use super::context::BodyResolutionContext;
use super::expressions::resolve_expression;
use super::members::resolve_member_call;
use super::utils::{
    create_struct_type, format_type, get_callable_behavior, is_expression_kind, matches_signature,
};

/// Resolve a call expression: callee(arg1, arg2, ...)
pub fn resolve_call_expression(
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
pub fn resolve_call(
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
                    // Functions are not mutable lvalues
                    let resolved_callee = Expression::symbol_ref(candidate_id, callee.ty.clone(), false, callee.span.clone());
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
pub fn resolve_struct_instantiation(
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

    // Verify it can be downcast to StructSymbol
    if symbol.as_ref().downcast_ref::<StructSymbol>().is_none() {
        return Expression::error(span);
    }

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
                // The return type is the actual struct type
                // Create a struct type from the struct symbol
                // We use Ty::named which stores the symbol ID for lookup
                let struct_ty = create_struct_type(&struct_symbol, span.clone());

                // For explicit init, create a Call expression
                // Initializers are not mutable lvalues
                let init_id = init_sym.metadata().id();
                let init_ref = Expression::symbol_ref(init_id, Ty::inferred(span.clone()), false, span.clone());
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
    // Create the actual struct type using the struct symbol
    let struct_ty = create_struct_type(&struct_symbol, span.clone());

    Expression::implicit_struct_init(struct_ty, arguments, span)
}

/// Resolve a method call from a MethodRef expression
pub fn resolve_method_call(
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
pub fn collect_overload_descriptions(candidates: &[SymbolId], db: &dyn Db) -> Vec<OverloadDescription> {
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
