//! Member access resolution.
//!
//! This module handles resolving member access expressions (field access, method calls)
//! including visibility checking and member chain resolution.

use std::sync::Arc;

use kestrel_reporting::IntoDiagnostic;
use kestrel_semantic_tree::behavior::member_access::MemberAccessBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::expr::{CallArgument, Expression, PrimitiveMethod};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_span::Span;
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::diagnostics::{
    CannotAccessMemberOnTypeError, MemberNotAccessibleError, MemberNotVisibleError,
    NoMatchingMethodError, NoSuchMemberError, NoSuchMethodError, PrimitiveMethodNotCallableError,
};
use crate::resolution::visibility::is_visible_from;

use super::calls::collect_overload_descriptions;
use super::context::BodyResolutionContext;
use super::utils::{
    format_symbol_kind, format_type, get_callable_behavior, get_type_container, matches_signature,
};

/// Resolve a chain of member accesses: obj.field1.field2.field3
pub fn resolve_member_chain(
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
pub fn resolve_member_access(
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

/// Resolve a member call from a FieldAccess expression: obj.method(args)
pub fn resolve_member_call(
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
