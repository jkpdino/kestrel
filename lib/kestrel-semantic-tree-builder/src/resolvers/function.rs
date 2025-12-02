use std::sync::Arc;

use kestrel_semantic_tree::behavior::callable::CallableBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::function::{FunctionSymbol, Parameter};
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::body_resolver::{resolve_function_body, BodyResolutionContext};
use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause};
use crate::type_resolver::{resolve_type_with_diagnostics, TypeResolutionContext};
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope, get_node_span,
    get_visibility_span, parse_visibility,
};

/// Resolver for function declarations
pub struct FunctionResolver;

impl Resolver for FunctionResolver {
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

        // Check if this function is static
        let is_static = syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::StaticModifier);

        // Check if this function has a body
        let has_body = syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::FunctionBody);

        // Extract parameters
        let parameters = extract_parameters(syntax, source);

        // Extract return type
        let return_type = extract_return_type(syntax, source);

        // Create the name object
        let name = Spanned::new(name_str, name_span);

        // Extract type parameters (they'll have function as parent later)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Create the function symbol with type parameters and where clause
        let function_symbol = FunctionSymbol::with_generics(
            name,
            full_span,
            visibility_behavior,
            is_static,
            has_body,
            parameters,
            return_type,
            type_parameters.clone(),
            where_clause,
            parent.cloned(),
        );
        let function_arc = Arc::new(function_symbol);
        let function_arc_dyn = function_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add type parameters as children of the function (not the module)
        // This ensures type parameters are in scope during type resolution
        add_type_params_as_children(&type_parameters, &function_arc_dyn);

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&function_arc_dyn);
        }

        Some(function_arc)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        syntax: &SyntaxNode,
        context: &mut BindingContext,
    ) {
        // Only process function symbols
        if symbol.metadata().kind() != KestrelSymbolKind::Function {
            return;
        }

        let symbol_id = symbol.metadata().id();
        let span = symbol.metadata().span().clone();

        // Get file_id for this symbol
        let file_id = context.file_id_for_symbol(symbol).unwrap_or(context.file_id);

        // Get source file name for looking up source later
        let source_file = context.source_file_name(symbol);
        let source = source_file
            .as_ref()
            .and_then(|name| context.sources.get(name))
            .map(|s| s.as_str())
            .unwrap_or("");

        // Extract and resolve parameters from syntax
        let resolved_params = resolve_parameters_from_syntax(syntax, source, symbol_id, context, file_id);

        // Extract and resolve return type from syntax
        let resolved_return = resolve_return_type_from_syntax(syntax, source, symbol_id, context, file_id);

        // Update the callable behavior with resolved types
        if let Some(func_sym) = symbol.as_ref().downcast_ref::<FunctionSymbol>() {
            let resolved_callable = CallableBehavior::new(resolved_params, resolved_return, span);
            func_sym.set_callable(resolved_callable);
        }
    }
}

/// Extract parameters from a FunctionDeclaration syntax node
fn extract_parameters(syntax: &SyntaxNode, source: &str) -> Vec<Parameter> {
    // Find the ParameterList node
    let param_list = match find_child(syntax, SyntaxKind::ParameterList) {
        Some(node) => node,
        None => return Vec::new(),
    };

    // Extract each Parameter node
    param_list
        .children()
        .filter(|child| child.kind() == SyntaxKind::Parameter)
        .filter_map(|param_node| extract_single_parameter(&param_node, source))
        .collect()
}

/// Extract a single parameter from a Parameter syntax node
///
/// Parameter structure:
/// - Parameter
///   - Name (label, optional - if there are 2 Name nodes, first is label)
///   - Name (bind_name)
///   - Colon
///   - Ty
fn extract_single_parameter(param_node: &SyntaxNode, source: &str) -> Option<Parameter> {
    // Collect all Name nodes
    let name_nodes: Vec<SyntaxNode> = param_node
        .children()
        .filter(|child| child.kind() == SyntaxKind::Name)
        .collect();

    if name_nodes.is_empty() {
        return None;
    }

    // Helper function to extract identifier text from a Name node
    fn extract_identifier_from_name(name_node: &SyntaxNode) -> Option<String> {
        name_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tok| tok.kind() == SyntaxKind::Identifier)
            .map(|tok| tok.text().to_string())
    }

    // Extract name strings and spans
    let (label, bind_name) = if name_nodes.len() >= 2 {
        // Two names: first is label, second is bind_name
        let label_str = extract_identifier_from_name(&name_nodes[0])?;
        let label_span = get_node_span(&name_nodes[0], source);
        let label = Spanned::new(label_str, label_span);

        let bind_str = extract_identifier_from_name(&name_nodes[1])?;
        let bind_span = get_node_span(&name_nodes[1], source);
        let bind_name = Spanned::new(bind_str, bind_span);

        (Some(label), bind_name)
    } else {
        // One name: it's the bind_name
        let bind_str = extract_identifier_from_name(&name_nodes[0])?;
        let bind_span = get_node_span(&name_nodes[0], source);
        let bind_name = Spanned::new(bind_str, bind_span);

        (None, bind_name)
    };

    // Extract the type
    let ty = extract_type_from_node(param_node, source);

    Some(match label {
        Some(l) => Parameter::with_label(l, bind_name, ty),
        None => Parameter::new(bind_name, ty),
    })
}

/// Extract return type from a FunctionDeclaration syntax node
fn extract_return_type(syntax: &SyntaxNode, source: &str) -> Ty {
    // Find the ReturnType node
    if let Some(ret_node) = find_child(syntax, SyntaxKind::ReturnType) {
        // Get the Ty child
        if let Some(ty_node) = find_child(&ret_node, SyntaxKind::Ty) {
            return extract_type_from_ty_node(&ty_node, source);
        }
    }

    // Default to unit type if no return type specified
    let fn_span = get_node_span(syntax, source);
    Ty::unit(fn_span.end..fn_span.end)
}

/// Extract type from a node that contains a Ty child
fn extract_type_from_node(node: &SyntaxNode, source: &str) -> Ty {
    if let Some(ty_node) = find_child(node, SyntaxKind::Ty) {
        return extract_type_from_ty_node(&ty_node, source);
    }

    Ty::error(0..0)
}

/// Extract type from a Ty syntax node
fn extract_type_from_ty_node(ty_node: &SyntaxNode, source: &str) -> Ty {
    let ty_span = get_node_span(ty_node, source);

    // Try TyPath
    if let Some(ty_path_node) = ty_node
        .children()
        .find(|child| child.kind() == SyntaxKind::TyPath)
    {
        // Find the Path node inside TyPath
        if let Some(path_node) = ty_path_node
            .children()
            .find(|child| child.kind() == SyntaxKind::Path)
        {
            // Collect path segments
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
        // Extract parameter types from TyList
        let mut param_types = Vec::new();
        if let Some(ty_list) = fn_ty_node
            .children()
            .find(|child| child.kind() == SyntaxKind::TyList)
        {
            for param_ty_node in ty_list.children().filter(|c| c.kind() == SyntaxKind::Ty) {
                param_types.push(extract_type_from_ty_node(&param_ty_node, source));
            }
        }

        // Extract return type (last Ty child of TyFunction, not in TyList)
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

/// Resolve parameters from a FunctionDeclaration syntax node during bind phase
fn resolve_parameters_from_syntax(
    syntax: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Vec<Parameter> {
    use crate::queries::TypePathResolution;

    // Find the ParameterList node
    let param_list = match find_child(syntax, SyntaxKind::ParameterList) {
        Some(node) => node,
        None => return Vec::new(),
    };

    // Extract and resolve each parameter
    param_list
        .children()
        .filter(|child| child.kind() == SyntaxKind::Parameter)
        .filter_map(|param_node| resolve_single_parameter(&param_node, source, context_id, ctx, file_id))
        .collect()
}

/// Resolve a single parameter from syntax
fn resolve_single_parameter(
    param_node: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Option<Parameter> {
    use crate::queries::TypePathResolution;

    // Collect all Name nodes
    let name_nodes: Vec<SyntaxNode> = param_node
        .children()
        .filter(|child| child.kind() == SyntaxKind::Name)
        .collect();

    if name_nodes.is_empty() {
        return None;
    }

    // Helper function to extract identifier text from a Name node
    fn extract_identifier_from_name(name_node: &SyntaxNode) -> Option<String> {
        name_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tok| tok.kind() == SyntaxKind::Identifier)
            .map(|tok| tok.text().to_string())
    }

    // Determine label and bind_name based on number of Name nodes
    let (label, bind_name) = if name_nodes.len() >= 2 {
        // Two names: first is label, second is bind_name
        let label_name = extract_identifier_from_name(&name_nodes[0]);
        let bind_name = Spanned::new(
            extract_identifier_from_name(&name_nodes[1])?,
            get_node_span(&name_nodes[1], source),
        );
        (label_name.map(|n| Spanned::new(n, get_node_span(&name_nodes[0], source))), bind_name)
    } else {
        // One name: no label, it's the bind_name
        let bind_name = Spanned::new(
            extract_identifier_from_name(&name_nodes[0])?,
            get_node_span(&name_nodes[0], source),
        );
        (None, bind_name)
    };

    // Find and resolve the type from Ty node
    let ty = if let Some(ty_node) = param_node.children().find(|c| c.kind() == SyntaxKind::Ty) {
        resolve_type_from_ty_node(&ty_node, source, context_id, ctx, file_id)
    } else {
        // No type annotation - inferred
        Ty::inferred(get_node_span(param_node, source))
    };

    Some(Parameter { label, bind_name, ty })
}

/// Resolve a type from a Ty syntax node during bind phase
fn resolve_type_from_ty_node(
    ty_node: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Ty {
    use crate::queries::TypePathResolution;

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
                // Resolve the path immediately
                match ctx.db.resolve_type_path(segments.clone(), context_id) {
                    TypePathResolution::Resolved(resolved_ty) => {
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
                param_types.push(resolve_type_from_ty_node(&param_ty_node, source, context_id, ctx, file_id));
            }
        }

        let return_ty = fn_ty_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .last()
            .map(|ty| resolve_type_from_ty_node(&ty, source, context_id, ctx, file_id))
            .unwrap_or_else(|| Ty::unit(ty_span.clone()));

        return Ty::function(param_types, return_ty, ty_span);
    }

    // Try TyTuple - recursively resolve nested types
    if let Some(tuple_node) = ty_node.children().find(|child| child.kind() == SyntaxKind::TyTuple) {
        let element_types: Vec<Ty> = tuple_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .map(|ty| resolve_type_from_ty_node(&ty, source, context_id, ctx, file_id))
            .collect();

        return Ty::tuple(element_types, ty_span);
    }

    // Fallback: error type
    Ty::error(ty_span)
}

/// Resolve return type from a FunctionDeclaration syntax node during bind phase
fn resolve_return_type_from_syntax(
    syntax: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Ty {
    // Find the return type node: FunctionDeclaration -> ReturnType -> Ty
    if let Some(return_type_node) = find_child(syntax, SyntaxKind::ReturnType) {
        if let Some(ty_node) = find_child(&return_type_node, SyntaxKind::Ty) {
            return resolve_type_from_ty_node(&ty_node, source, context_id, ctx, file_id);
        }
    }

    // No explicit return type - defaults to unit
    let fn_span = get_node_span(syntax, source);
    Ty::unit(fn_span.end..fn_span.end)
}
