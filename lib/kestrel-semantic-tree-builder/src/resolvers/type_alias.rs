use std::sync::Arc;

use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::type_alias::{TypeAliasSymbol, TypeAliasTypedBehavior};
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::queries::TypePathResolution;
use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause};
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope, get_node_span,
    get_visibility_span, parse_visibility,
};

/// Resolver for type alias declarations
pub struct TypeAliasResolver;

impl Resolver for TypeAliasResolver {
    fn build_declaration(
        &self,
        syntax: &SyntaxNode,
        source: &str,
        parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        // Extract name
        let name_str = extract_name(syntax)?;
        let name_node = find_child(syntax, SyntaxKind::Name)?;
        let name_span = get_node_span(&name_node, source);

        // Get full span
        let full_span = get_node_span(syntax, source);

        // Extract visibility
        let visibility_str = extract_visibility(syntax);
        let visibility_enum = visibility_str.as_deref().and_then(parse_visibility);

        let visibility_span = get_visibility_span(syntax, source).unwrap_or(name_span.clone());

        // Determine visibility scope
        let visibility_scope = find_visibility_scope(visibility_enum.as_ref(), parent, root);

        // Create visibility behavior
        let visibility_behavior =
            VisibilityBehavior::new(visibility_enum, visibility_span, visibility_scope);

        // Create the name object
        let name = Spanned::new(name_str, name_span);

        // Use error type as placeholder - actual type will be resolved during bind from syntax
        let placeholder_type = Ty::error(full_span.clone());
        let syntactic_typed_behavior = TypedBehavior::new(placeholder_type, full_span.clone());

        // Extract type parameters (they'll have type alias as parent later)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Create the type alias symbol with type parameters and where clause
        let type_alias_symbol = TypeAliasSymbol::with_generics(
            name.clone(),
            full_span.clone(),
            visibility_behavior,
            syntactic_typed_behavior,
            type_parameters.clone(),
            where_clause,
            parent.cloned(),
        );
        let type_alias_arc = Arc::new(type_alias_symbol);

        // The type of a type alias symbol is TypeAlias (referring to itself)
        // This is what allows type checkers to distinguish type aliases from their underlying types
        let type_alias_type =
            kestrel_semantic_tree::ty::Ty::type_alias(type_alias_arc.clone(), full_span.clone());
        let semantic_typed_behavior = TypedBehavior::new(type_alias_type, full_span.clone());

        // Add a second TypedBehavior with the TypeAlias type
        // Now the symbol has two TypedBehaviors:
        // 1. The first one (from new()) contains the syntactic aliased type
        // 2. This one contains the TypeAlias type
        type_alias_arc
            .metadata()
            .add_behavior(semantic_typed_behavior);

        let type_alias_arc_dyn = type_alias_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add type parameters as children of the type alias (not the module)
        // This ensures type parameters are in scope during type resolution
        add_type_params_as_children(&type_parameters, &type_alias_arc_dyn);

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&type_alias_arc_dyn);
        }

        Some(type_alias_arc_dyn)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        syntax: &SyntaxNode,
        context: &mut BindingContext,
    ) {
        let symbol_id = symbol.metadata().id();

        // Enter the cycle detector to track that we're binding this type alias
        // This shouldn't fail on first entry, but we check anyway
        if context.type_alias_cycle_detector.enter(symbol_id).is_err() {
            // We're already binding this type alias - this shouldn't happen
            // in normal sequential binding, but guard against it
            return;
        }

        // Get file_id for this symbol
        let file_id = context.file_id_for_symbol(symbol).unwrap_or(context.file_id);

        // Get source for this symbol's file
        let source_file = context.source_file_name(symbol);
        let source = source_file
            .as_ref()
            .and_then(|name| context.sources.get(name))
            .map(|s| s.as_str())
            .unwrap_or("");

        // Extract and resolve the aliased type from syntax
        let resolved_type = resolve_aliased_type_from_syntax(syntax, source, symbol_id, context, file_id);

        // Add the resolved type as a TypeAliasTypedBehavior
        let type_alias_typed_behavior = TypeAliasTypedBehavior::new(resolved_type);
        symbol.metadata().add_behavior(type_alias_typed_behavior);

        // Exit the cycle detector after we're done resolving this type alias
        context.type_alias_cycle_detector.exit();
    }
}

/// Resolve the aliased type from a TypeAliasDeclaration syntax node during bind phase
fn resolve_aliased_type_from_syntax(
    syntax: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Ty {
    // Find the AliasedType node
    let aliased_type_node = match find_child(syntax, SyntaxKind::AliasedType) {
        Some(node) => node,
        None => return Ty::error(get_node_span(syntax, source)),
    };

    // Try to find a Ty node first. If it doesn't exist, use the AliasedType node itself.
    let ty_node = find_child(&aliased_type_node, SyntaxKind::Ty)
        .unwrap_or_else(|| aliased_type_node.clone());

    resolve_type_from_syntax(&ty_node, source, context_id, ctx, file_id)
}

/// Resolve a type from a Ty syntax node during bind phase
fn resolve_type_from_syntax(
    ty_node: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Ty {
    let ty_span = get_node_span(ty_node, source);

    // Handle Ty wrapper node
    if ty_node.kind() == SyntaxKind::Ty {
        if let Some(child) = ty_node.children().next() {
            return resolve_type_from_syntax(&child, source, context_id, ctx, file_id);
        }
    }

    // Try TyPath
    if ty_node.kind() == SyntaxKind::TyPath {
        if let Some(path_node) = ty_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Path)
        {
            let segments: Vec<String> = path_node
                .children()
                .filter(|child| child.kind() == SyntaxKind::PathElement)
                .filter_map(|path_elem| {
                    path_elem
                        .children_with_tokens()
                        .filter_map(|elem| elem.into_token())
                        .find(|tok| tok.kind() == SyntaxKind::Identifier)
                        .map(|tok| tok.text().to_string())
                })
                .collect();

            if !segments.is_empty() {
                // Check for type arguments
                let type_args = extract_type_arguments(&ty_node, source, context_id, ctx, file_id);

                // Resolve the path
                match ctx.db.resolve_type_path(segments.clone(), context_id) {
                    TypePathResolution::Resolved(mut resolved_ty) => {
                        // Apply type arguments if present
                        if !type_args.is_empty() {
                            resolved_ty = apply_type_args_to_resolved(resolved_ty, type_args, ty_span.clone(), ctx, file_id);
                        }
                        return resolved_ty;
                    }
                    TypePathResolution::NotFound { segment, .. } => {
                        let diagnostic = kestrel_reporting::Diagnostic::error()
                            .with_message(format!("cannot find type '{}' in this scope", segment))
                            .with_labels(vec![kestrel_reporting::Label::primary(file_id, ty_span.clone())
                                .with_message("not found")]);
                        ctx.diagnostics.add_diagnostic(diagnostic);
                        return Ty::error(ty_span);
                    }
                    TypePathResolution::Ambiguous { segment, candidates, .. } => {
                        let diagnostic = kestrel_reporting::Diagnostic::error()
                            .with_message(format!("type '{}' is ambiguous ({} candidates)", segment, candidates.len()))
                            .with_labels(vec![kestrel_reporting::Label::primary(file_id, ty_span.clone())
                                .with_message("ambiguous")]);
                        ctx.diagnostics.add_diagnostic(diagnostic);
                        return Ty::error(ty_span);
                    }
                    TypePathResolution::NotAType { .. } => {
                        let diagnostic = kestrel_reporting::Diagnostic::error()
                            .with_message(format!("'{}' is not a type", segments.join(".")))
                            .with_labels(vec![kestrel_reporting::Label::primary(file_id, ty_span.clone())
                                .with_message("not a type")]);
                        ctx.diagnostics.add_diagnostic(diagnostic);
                        return Ty::error(ty_span);
                    }
                }
            }
        }
    }

    // Try TyUnit
    if ty_node.kind() == SyntaxKind::TyUnit {
        return Ty::unit(ty_span);
    }

    // Try TyNever
    if ty_node.kind() == SyntaxKind::TyNever {
        return Ty::never(ty_span);
    }

    // Try TyFunction
    if ty_node.kind() == SyntaxKind::TyFunction {
        let mut param_types = Vec::new();
        if let Some(ty_list) = ty_node.children().find(|child| child.kind() == SyntaxKind::TyList) {
            for param_ty_node in ty_list.children().filter(|c| c.kind() == SyntaxKind::Ty) {
                param_types.push(resolve_type_from_syntax(&param_ty_node, source, context_id, ctx, file_id));
            }
        }

        let return_ty = ty_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .last()
            .map(|ty| resolve_type_from_syntax(&ty, source, context_id, ctx, file_id))
            .unwrap_or_else(|| Ty::unit(ty_span.clone()));

        return Ty::function(param_types, return_ty, ty_span);
    }

    // Try TyTuple
    if ty_node.kind() == SyntaxKind::TyTuple {
        let element_types: Vec<Ty> = ty_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .map(|ty| resolve_type_from_syntax(&ty, source, context_id, ctx, file_id))
            .collect();

        return Ty::tuple(element_types, ty_span);
    }

    // Fallback: error type
    Ty::error(ty_span)
}

/// Extract type arguments from a TyPath node
fn extract_type_arguments(
    ty_path_node: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Vec<Ty> {
    // Look for TypeArgumentList child
    if let Some(arg_list) = ty_path_node.children().find(|c| c.kind() == SyntaxKind::TypeArgumentList) {
        arg_list
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .map(|ty| resolve_type_from_syntax(&ty, source, context_id, ctx, file_id))
            .collect()
    } else {
        Vec::new()
    }
}

/// Apply type arguments to a resolved type
fn apply_type_args_to_resolved(
    resolved_ty: Ty,
    type_args: Vec<Ty>,
    span: kestrel_span::Span,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Ty {
    use kestrel_semantic_tree::ty::{TyKind, Substitutions};

    match resolved_ty.kind() {
        TyKind::Struct { symbol, .. } => {
            let type_params = symbol.type_parameters();
            let max_args = type_params.len();
            let min_args = type_params.iter().filter(|p| !p.has_default()).count();
            let actual = type_args.len();

            // Non-generic struct with type args
            if max_args == 0 {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("type '{}' does not accept type arguments", symbol.metadata().name().value))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                return Ty::error(span);
            }

            // Check arity with defaults
            if actual < min_args {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("too few type arguments: expected at least {}, found {}", min_args, actual))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                return Ty::error(span);
            }
            if actual > max_args {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("too many type arguments: expected at most {}, found {}", max_args, actual))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
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

            Ty::generic_struct(symbol.clone(), substitutions, span)
        }
        TyKind::Protocol { symbol, .. } => {
            let type_params = symbol.type_parameters();
            let max_args = type_params.len();
            let min_args = type_params.iter().filter(|p| !p.has_default()).count();
            let actual = type_args.len();

            // Non-generic protocol with type args
            if max_args == 0 {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("type '{}' does not accept type arguments", symbol.metadata().name().value))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                return Ty::error(span);
            }

            if actual < min_args {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("too few type arguments: expected at least {}, found {}", min_args, actual))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                return Ty::error(span);
            }
            if actual > max_args {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("too many type arguments: expected at most {}, found {}", max_args, actual))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                return Ty::error(span);
            }

            let mut substitutions = Substitutions::new();
            for (i, param) in type_params.iter().enumerate() {
                let ty = if i < type_args.len() {
                    type_args[i].clone()
                } else if let Some(default) = param.default() {
                    default.clone()
                } else {
                    Ty::error(span.clone())
                };
                substitutions.insert(param.metadata().id(), ty);
            }

            Ty::generic_protocol(symbol.clone(), substitutions, span)
        }
        TyKind::TypeAlias { symbol, .. } => {
            let type_params = symbol.type_parameters();
            let max_args = type_params.len();
            let min_args = type_params.iter().filter(|p| !p.has_default()).count();
            let actual = type_args.len();

            // Non-generic type alias with type args
            if max_args == 0 {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("type '{}' does not accept type arguments", symbol.metadata().name().value))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                return Ty::error(span);
            }

            if actual < min_args {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("too few type arguments: expected at least {}, found {}", min_args, actual))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                return Ty::error(span);
            }
            if actual > max_args {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("too many type arguments: expected at most {}, found {}", max_args, actual))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                return Ty::error(span);
            }

            let mut substitutions = Substitutions::new();
            for (i, param) in type_params.iter().enumerate() {
                let ty = if i < type_args.len() {
                    type_args[i].clone()
                } else if let Some(default) = param.default() {
                    default.clone()
                } else {
                    Ty::error(span.clone())
                };
                substitutions.insert(param.metadata().id(), ty);
            }

            Ty::generic_type_alias(symbol.clone(), substitutions, span)
        }
        // Non-generic types with type arguments is an error
        _ => {
            // Get a name for the error message
            let type_name = match resolved_ty.kind() {
                TyKind::Int(_) => "Int".to_string(),
                TyKind::Float(_) => "Float".to_string(),
                TyKind::Bool => "Bool".to_string(),
                TyKind::String => "String".to_string(),
                TyKind::Unit => "()".to_string(),
                TyKind::Never => "Never".to_string(),
                _ => "type".to_string(),
            };
            let diagnostic = kestrel_reporting::Diagnostic::error()
                .with_message(format!("type '{}' does not accept type arguments", type_name))
                .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
            ctx.diagnostics.add_diagnostic(diagnostic);
            Ty::error(span)
        }
    }
}