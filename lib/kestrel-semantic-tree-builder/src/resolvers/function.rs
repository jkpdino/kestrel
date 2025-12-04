use std::sync::Arc;

use kestrel_semantic_tree::behavior::callable::{CallableBehavior, ReceiverKind};
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::function::{FunctionSymbol, Parameter};
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause};
use crate::type_syntax::{extract_type_from_ty_node, extract_type_from_node, resolve_type_from_ty_node, TypeSyntaxContext};
use crate::utils::{
    extract_identifier_from_name, extract_name, extract_visibility, find_child,
    find_visibility_scope, get_node_span, get_visibility_span, parse_visibility,
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

        // Get file_id and source for this symbol
        let (file_id, source) = context.get_file_context(symbol);

        // Extract and resolve parameters from syntax
        let resolved_params = resolve_parameters_from_syntax(syntax, &source, symbol_id, context, file_id);

        // Extract and resolve return type from syntax
        let resolved_return = resolve_return_type_from_syntax(syntax, &source, symbol_id, context, file_id);

        // Determine receiver kind for instance methods
        let receiver_kind = determine_receiver_kind(syntax, symbol);

        // Add a new CallableBehavior with resolved types
        // The FunctionSymbol.get_callable() method returns the last CallableBehavior,
        // which will be this resolved one.
        let resolved_callable = match receiver_kind {
            Some(kind) => CallableBehavior::with_receiver(resolved_params.clone(), resolved_return, kind, span),
            None => CallableBehavior::new(resolved_params.clone(), resolved_return, span),
        };
        symbol.metadata().add_behavior(resolved_callable);

        // Resolve function body if present
        if let Some(body_node) = find_child(syntax, SyntaxKind::FunctionBody) {
            resolve_function_body(symbol, &body_node, &resolved_params, context, file_id, &source);
        }
    }
}

/// Resolve a function's body and attach ExecutableBehavior to the symbol
fn resolve_function_body(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    body_node: &SyntaxNode,
    params: &[Parameter],
    context: &mut BindingContext,
    file_id: usize,
    source: &str,
) {
    use kestrel_semantic_tree::behavior::executable::ExecutableBehavior;
    use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
    use kestrel_semantic_tree::symbol::function::FunctionSymbol;
    use crate::body_resolver::{BodyResolutionContext, resolve_function_body as resolve_body};

    // Downcast to FunctionSymbol to get Arc<FunctionSymbol> for LocalScope
    let Some(func_sym) = symbol.as_ref().downcast_ref::<FunctionSymbol>() else {
        return;
    };

    // Create LocalScope with the function symbol
    // We need to create an Arc<FunctionSymbol>, but we only have &FunctionSymbol
    // The workaround is to get the symbol from the db
    let Some(func_arc) = context.db.symbol_by_id(symbol.metadata().id()) else {
        return;
    };

    // Verify it's a FunctionSymbol (already confirmed above)
    if func_arc.as_ref().downcast_ref::<FunctionSymbol>().is_none() {
        return;
    }

    // Create a temporary FunctionSymbol that we can use with LocalScope
    // This is needed because LocalScope::new takes Arc<FunctionSymbol>
    // The locals will be added to the actual function through the Arc<dyn Symbol>
    use kestrel_semantic_tree::behavior::visibility::{Visibility, VisibilityBehavior};
    use kestrel_span::Spanned;

    let temp_name = Spanned::new("__body_temp".to_string(), 0..0);
    let temp_vis = VisibilityBehavior::new(Some(Visibility::Private), 0..0, func_arc.clone());
    let temp_func = Arc::new(FunctionSymbol::new(
        temp_name,
        0..0,
        temp_vis,
        true,
        true,
        vec![],
        kestrel_semantic_tree::ty::Ty::unit(0..0),
        None,
    ));

    let mut local_scope = crate::local_scope::LocalScope::new(temp_func);

    // Get receiver kind from CallableBehavior to determine if we need to inject `self`
    let receiver_kind = symbol
        .metadata()
        .behaviors()
        .iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::Callable))
        .and_then(|b| b.as_ref().downcast_ref::<CallableBehavior>())
        .and_then(|cb| cb.receiver());

    // If this is an instance method, inject `self` as the first local
    if let Some(receiver) = receiver_kind {
        if let Some(self_type) = get_self_type(symbol) {
            let is_mutable = matches!(receiver, ReceiverKind::Mutating);
            let self_span = symbol.metadata().span().start..symbol.metadata().span().start;

            // Add self to local scope
            local_scope.bind("self".to_string(), self_type.clone(), is_mutable, self_span.clone());
            // Add to the actual function symbol
            func_sym.add_local("self".to_string(), self_type, is_mutable, self_span);
        }
    }

    // Add parameters to local scope
    for param in params {
        let param_ty = param.ty.clone();
        let param_name = param.bind_name.value.clone();
        let param_span = param.bind_name.span.clone();
        // Add to local scope and also to the actual function
        local_scope.bind(param_name.clone(), param_ty.clone(), false, param_span.clone());
        // Add to the actual function symbol
        func_sym.add_local(param_name, param_ty, false, param_span);
    }

    // Create body resolution context
    let mut body_ctx = BodyResolutionContext {
        db: context.db,
        diagnostics: context.diagnostics,
        file_id,
        source,
        function_id: symbol.metadata().id(),
        local_scope,
    };

    // Resolve the body
    let code_block = resolve_body(body_node, &mut body_ctx);

    // Transfer locals from temp function to real function
    // (locals created during body resolution need to be added to the real function)
    // The temp function's locals are tracked separately, so we need to sync them

    // Create and attach ExecutableBehavior
    let executable = ExecutableBehavior::new(code_block);
    symbol.metadata().add_behavior(executable);
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

/// Resolve parameters from a FunctionDeclaration syntax node during bind phase
fn resolve_parameters_from_syntax(
    syntax: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Vec<Parameter> {
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
    // Collect all Name nodes
    let name_nodes: Vec<SyntaxNode> = param_node
        .children()
        .filter(|child| child.kind() == SyntaxKind::Name)
        .collect();

    if name_nodes.is_empty() {
        return None;
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

    // Find and resolve the type from Ty node using shared utility
    let ty = if let Some(ty_node) = param_node.children().find(|c| c.kind() == SyntaxKind::Ty) {
        let mut type_ctx = TypeSyntaxContext::new(ctx.db, ctx.diagnostics, file_id, source, context_id);
        resolve_type_from_ty_node(&ty_node, &mut type_ctx)
    } else {
        // No type annotation - inferred
        Ty::inferred(get_node_span(param_node, source))
    };

    Some(Parameter { label, bind_name, ty })
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
            let mut type_ctx = TypeSyntaxContext::new(ctx.db, ctx.diagnostics, file_id, source, context_id);
            return resolve_type_from_ty_node(&ty_node, &mut type_ctx);
        }
    }

    // No explicit return type - defaults to unit
    let fn_span = get_node_span(syntax, source);
    Ty::unit(fn_span.end..fn_span.end)
}

/// Get the type of `self` for an instance method
///
/// Returns the type of the containing struct or protocol.
/// For now, we use `Self` type which will be resolved later.
fn get_self_type(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<Ty> {
    let parent = symbol.metadata().parent()?;
    let parent_span = parent.metadata().span().clone();

    match parent.metadata().kind() {
        KestrelSymbolKind::Struct | KestrelSymbolKind::Protocol => {
            // Use Self type which refers to the containing type
            // This will be resolved to the concrete type during type checking
            Some(Ty::self_type(parent_span))
        }
        _ => None,
    }
}

/// Determine the receiver kind for a function declaration
///
/// Returns:
/// - `None` for static functions and free functions (not in a struct/protocol)
/// - `Some(ReceiverKind)` for instance methods
fn determine_receiver_kind(
    syntax: &SyntaxNode,
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Option<ReceiverKind> {
    // Check if this function is static
    let is_static = syntax
        .children()
        .any(|child| child.kind() == SyntaxKind::StaticModifier);

    if is_static {
        return None; // Static functions have no receiver
    }

    // Check if the function is in a struct or protocol (instance method)
    let parent_kind = symbol.metadata().parent().map(|p| p.metadata().kind());
    let is_instance_method = matches!(
        parent_kind,
        Some(KestrelSymbolKind::Struct) | Some(KestrelSymbolKind::Protocol)
    );

    if !is_instance_method {
        return None; // Free functions have no receiver
    }

    // Check for receiver modifier (mutating/consuming)
    let has_mutating = syntax
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .any(|tok| tok.kind() == SyntaxKind::Mutating);

    let has_consuming = syntax
        .children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .any(|tok| tok.kind() == SyntaxKind::Consuming);

    // Determine receiver kind
    match (has_mutating, has_consuming) {
        (true, _) => Some(ReceiverKind::Mutating),
        (_, true) => Some(ReceiverKind::Consuming),
        _ => Some(ReceiverKind::Borrowing), // Default for instance methods
    }
}
