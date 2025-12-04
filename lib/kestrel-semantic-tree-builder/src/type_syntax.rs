//! Shared type-from-syntax resolution utilities
//!
//! This module provides a unified interface for resolving types from syntax nodes.
//! It consolidates the type resolution logic used by:
//! - `resolvers/function.rs` (parameter and return type resolution)
//! - `resolvers/field.rs` (field type resolution)
//! - `resolvers/type_alias.rs` (aliased type resolution)

use kestrel_prelude::primitives;
use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::ty::{Substitutions, Ty, TyKind};
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::diagnostics::{
    AmbiguousTypeError, NotATypeError, NotGenericError,
    TooFewTypeArgumentsError, TooManyTypeArgumentsError, UnresolvedTypeError,
};
use crate::queries::{Db, TypePathResolution};
use crate::utils::{extract_path_segments, get_node_span};

/// Context for type resolution from syntax during the bind phase
pub struct TypeSyntaxContext<'a> {
    /// Database for resolving type paths
    pub db: &'a dyn Db,
    /// Diagnostics for error reporting
    pub diagnostics: &'a mut DiagnosticContext,
    /// File ID for error reporting
    pub file_id: usize,
    /// Source code for span calculation
    pub source: &'a str,
    /// Symbol ID of the context for resolution
    pub context_id: SymbolId,
}

impl<'a> TypeSyntaxContext<'a> {
    /// Create a new TypeSyntaxContext
    pub fn new(
        db: &'a dyn Db,
        diagnostics: &'a mut DiagnosticContext,
        file_id: usize,
        source: &'a str,
        context_id: SymbolId,
    ) -> Self {
        Self {
            db,
            diagnostics,
            file_id,
            source,
            context_id,
        }
    }
}

/// Extract a type from a Ty syntax node without resolution (placeholder types).
///
/// This is used during the build phase when we don't have access to the database.
/// Type paths are returned as error types - they will be resolved during bind phase.
pub fn extract_type_from_ty_node(ty_node: &SyntaxNode, source: &str) -> Ty {
    let ty_span = get_node_span(ty_node, source);

    // Try TyPath
    if let Some(ty_path_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyPath)
    {
        if let Some(path_node) = ty_path_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Path)
        {
            let segments: Vec<String> = extract_path_segments(&path_node);
            if !segments.is_empty() {
                // Return error as placeholder - will be resolved during bind
                return Ty::error(ty_span);
            }
        }
    }

    // Try TyUnit
    if ty_node
        .children()
        .any(|child| child.kind() == SyntaxKind::TyUnit)
    {
        return Ty::unit(ty_span);
    }

    // Try TyNever
    if ty_node
        .children()
        .any(|child| child.kind() == SyntaxKind::TyNever)
    {
        return Ty::never(ty_span);
    }

    // Try TyFunction
    if let Some(fn_ty_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyFunction)
    {
        let mut param_types = Vec::new();
        if let Some(ty_list) = fn_ty_node
            .children()
            .find(|child| child.kind() == SyntaxKind::TyList)
        {
            for param_ty_node in ty_list.children().filter(|c| c.kind() == SyntaxKind::Ty) {
                param_types.push(extract_type_from_ty_node(&param_ty_node, source));
            }
        }

        let return_ty = fn_ty_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .last()
            .map(|ty| extract_type_from_ty_node(&ty, source))
            .unwrap_or_else(|| Ty::unit(ty_span.clone()));

        return Ty::function(param_types, return_ty, ty_span);
    }

    // Try TyTuple
    if let Some(tuple_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyTuple)
    {
        let element_types: Vec<Ty> = tuple_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .map(|ty| extract_type_from_ty_node(&ty, source))
            .collect();

        return Ty::tuple(element_types, ty_span);
    }

    // Fallback: error type
    Ty::error(ty_span)
}

/// Resolve a type from a Ty syntax node during bind phase.
///
/// This resolves type paths immediately using the database.
/// Emits diagnostics for resolution errors.
/// Handles type arguments for generic types.
pub fn resolve_type_from_ty_node(ty_node: &SyntaxNode, ctx: &mut TypeSyntaxContext) -> Ty {
    let ty_span = get_node_span(ty_node, ctx.source);

    // Try TyPath (with type arguments support)
    if let Some(ty_path_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyPath)
    {
        return resolve_ty_path(&ty_path_node, ctx);
    }

    // Try TyUnit
    if ty_node.children().any(|child| child.kind() == SyntaxKind::TyUnit) {
        return Ty::unit(ty_span);
    }

    // Try TyNever
    if ty_node.children().any(|child| child.kind() == SyntaxKind::TyNever) {
        return Ty::never(ty_span);
    }

    // Try TyFunction - recursively resolve nested types
    if let Some(fn_ty_node) = ty_node.children().find(|child| child.kind() == SyntaxKind::TyFunction) {
        let mut param_types = Vec::new();
        if let Some(ty_list) = fn_ty_node.children().find(|child| child.kind() == SyntaxKind::TyList) {
            for param_ty_node in ty_list.children().filter(|c| c.kind() == SyntaxKind::Ty) {
                param_types.push(resolve_type_from_ty_node(&param_ty_node, ctx));
            }
        }

        let return_ty = fn_ty_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .last()
            .map(|ty| resolve_type_from_ty_node(&ty, ctx))
            .unwrap_or_else(|| Ty::unit(ty_span.clone()));

        return Ty::function(param_types, return_ty, ty_span);
    }

    // Try TyTuple - recursively resolve nested types
    if let Some(tuple_node) = ty_node.children().find(|child| child.kind() == SyntaxKind::TyTuple) {
        let element_types: Vec<Ty> = tuple_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .map(|ty| resolve_type_from_ty_node(&ty, ctx))
            .collect();

        return Ty::tuple(element_types, ty_span);
    }

    // Try TyArray - recursively resolve element type
    if let Some(array_node) = ty_node.children().find(|child| child.kind() == SyntaxKind::TyArray) {
        if let Some(element_ty_node) = array_node.children().find(|c| c.kind() == SyntaxKind::Ty) {
            let element_ty = resolve_type_from_ty_node(&element_ty_node, ctx);
            return Ty::array(element_ty, ty_span);
        }
        return Ty::error(ty_span);
    }

    // Fallback: error type
    Ty::error(ty_span)
}

/// Resolve a type path and emit diagnostics on failure
fn resolve_type_path(
    segments: &[String],
    ty_span: kestrel_span::Span,
    ctx: &mut TypeSyntaxContext,
) -> Ty {
    match ctx.db.resolve_type_path(segments.to_vec(), ctx.context_id) {
        TypePathResolution::Resolved(resolved_ty) => resolved_ty,
        TypePathResolution::NotFound { segment, .. } => {
            ctx.diagnostics.throw(UnresolvedTypeError {
                span: ty_span.clone(),
                type_name: segment,
            }, ctx.file_id);
            Ty::error(ty_span)
        }
        TypePathResolution::Ambiguous { segment, candidates, .. } => {
            ctx.diagnostics.throw(AmbiguousTypeError {
                span: ty_span.clone(),
                type_name: segment,
                candidate_count: candidates.len(),
            }, ctx.file_id);
            Ty::error(ty_span)
        }
        TypePathResolution::NotAType { .. } => {
            ctx.diagnostics.throw(NotATypeError {
                span: ty_span.clone(),
                name: segments.join("."),
            }, ctx.file_id);
            Ty::error(ty_span)
        }
    }
}

/// Extract type arguments from a TyPath node
fn extract_type_arguments(
    ty_path_node: &SyntaxNode,
    ctx: &mut TypeSyntaxContext,
) -> Vec<Ty> {
    if let Some(arg_list) = ty_path_node.children().find(|c| c.kind() == SyntaxKind::TypeArgumentList) {
        arg_list
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .map(|ty| resolve_type_from_ty_node(&ty, ctx))
            .collect()
    } else {
        Vec::new()
    }
}

/// Apply type arguments to a resolved type.
///
/// This is the unified function for applying type arguments to generic types.
/// It handles Struct, Protocol, and TypeAlias types, checking arity and building
/// substitutions with support for default type arguments.
pub fn apply_type_arguments(
    resolved_ty: &Ty,
    type_args: Vec<Ty>,
    span: kestrel_span::Span,
    ctx: &mut TypeSyntaxContext,
) -> Ty {
    match resolved_ty.kind() {
        TyKind::Struct { symbol, .. } => {
            let type_params = symbol.type_parameters();
            let type_name = symbol.metadata().name().value.clone();
            apply_type_args_to_generic(
                type_params,
                &type_name,
                type_args,
                span.clone(),
                ctx,
                |subs| Ty::generic_struct(symbol.clone(), subs, span),
            )
        }

        TyKind::Protocol { symbol, .. } => {
            let type_params = symbol.type_parameters();
            let type_name = symbol.metadata().name().value.clone();
            apply_type_args_to_generic(
                type_params,
                &type_name,
                type_args,
                span.clone(),
                ctx,
                |subs| Ty::generic_protocol(symbol.clone(), subs, span),
            )
        }

        TyKind::TypeAlias { symbol, .. } => {
            let type_params = symbol.type_parameters();
            let type_name = symbol.metadata().name().value.clone();
            apply_type_args_to_generic(
                type_params,
                &type_name,
                type_args,
                span.clone(),
                ctx,
                |subs| Ty::generic_type_alias(symbol.clone(), subs, span),
            )
        }

        // Non-generic types with type arguments is an error
        _ => {
            let type_name = match resolved_ty.kind() {
                TyKind::Int(_) => primitives::INT.to_string(),
                TyKind::Float(_) => primitives::FLOAT.to_string(),
                TyKind::Bool => primitives::BOOL.to_string(),
                TyKind::String => primitives::STRING.to_string(),
                TyKind::Unit => "()".to_string(),
                TyKind::Never => "Never".to_string(),
                TyKind::TypeParameter(p) => p.metadata().name().value.clone(),
                _ => "type".to_string(),
            };
            ctx.diagnostics.throw(NotGenericError {
                span: span.clone(),
                type_name,
            }, ctx.file_id);
            Ty::error(span)
        }
    }
}

/// Helper to apply type arguments to a generic type (struct, protocol, or type alias)
fn apply_type_args_to_generic<F>(
    type_params: &[std::sync::Arc<kestrel_semantic_tree::symbol::type_parameter::TypeParameterSymbol>],
    type_name: &str,
    type_args: Vec<Ty>,
    span: kestrel_span::Span,
    ctx: &mut TypeSyntaxContext,
    make_ty: F,
) -> Ty
where
    F: FnOnce(Substitutions) -> Ty,
{
    let max_args = type_params.len();
    let min_args = type_params.iter().filter(|p| !p.has_default()).count();
    let actual = type_args.len();

    // Non-generic type with type args
    if max_args == 0 {
        ctx.diagnostics.throw(NotGenericError {
            span: span.clone(),
            type_name: type_name.to_string(),
        }, ctx.file_id);
        return Ty::error(span);
    }

    // Check arity with defaults
    if actual < min_args {
        ctx.diagnostics.throw(TooFewTypeArgumentsError {
            span: span.clone(),
            type_name: type_name.to_string(),
            min_expected: min_args,
            got: actual,
        }, ctx.file_id);
        return Ty::error(span);
    }
    if actual > max_args {
        ctx.diagnostics.throw(TooManyTypeArgumentsError {
            span: span.clone(),
            type_name: type_name.to_string(),
            max_expected: max_args,
            got: actual,
        }, ctx.file_id);
        return Ty::error(span);
    }

    // Build substitutions, using defaults for missing args
    let mut substitutions = Substitutions::new();
    for (i, param) in type_params.iter().enumerate() {
        let ty = if i < type_args.len() {
            type_args[i].clone()
        } else if let Some(default) = param.default() {
            default.clone()
        } else {
            // This shouldn't happen if arity check passed
            Ty::error(span.clone())
        };
        substitutions.insert(param.metadata().id(), ty);
    }

    make_ty(substitutions)
}

/// Resolve a TyPath node, handling type arguments if present.
///
/// This is used internally by resolve_type_from_ty_node to handle paths with type args.
fn resolve_ty_path(
    ty_path_node: &SyntaxNode,
    ctx: &mut TypeSyntaxContext,
) -> Ty {
    let ty_span = get_node_span(ty_path_node, ctx.source);

    if let Some(path_node) = ty_path_node
        .children()
        .find(|child| child.kind() == SyntaxKind::Path)
    {
        let segments = extract_path_segments(&path_node);

        if !segments.is_empty() {
            // Check for type arguments
            let type_args = extract_type_arguments(ty_path_node, ctx);

            // Resolve the path
            let resolved = resolve_type_path(&segments, ty_span.clone(), ctx);

            // Apply type arguments if present and path resolved
            if !type_args.is_empty() && !resolved.is_error() {
                return apply_type_arguments(&resolved, type_args, ty_span, ctx);
            }

            return resolved;
        }
    }

    Ty::error(ty_span)
}

/// Extract type from a node that contains a Ty child (without resolution)
pub fn extract_type_from_node(node: &SyntaxNode, source: &str) -> Ty {
    if let Some(ty_node) = node.children().find(|c| c.kind() == SyntaxKind::Ty) {
        return extract_type_from_ty_node(&ty_node, source);
    }
    Ty::error(0..0)
}

/// Resolve type from a node that contains a Ty child
pub fn resolve_type_from_node(node: &SyntaxNode, ctx: &mut TypeSyntaxContext) -> Ty {
    if let Some(ty_node) = node.children().find(|c| c.kind() == SyntaxKind::Ty) {
        return resolve_type_from_ty_node(&ty_node, ctx);
    }
    Ty::error(0..0)
}
