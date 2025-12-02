use std::sync::Arc;

use kestrel_semantic_tree::behavior::conformances::ConformancesBehavior;
use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::protocol::ProtocolSymbol;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::diagnostics::{NotAProtocolContext, NotAProtocolError, UnresolvedTypeError};
use crate::queries::TypePathResolution;
use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause};
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope, get_node_span,
    get_visibility_span, parse_visibility,
};

/// Resolver for protocol declarations
pub struct ProtocolResolver;

impl Resolver for ProtocolResolver {
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

        // Extract type parameters (they'll have protocol as parent later)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Create the protocol symbol with type parameters and where clause
        // Inherited protocols are resolved and added as a ConformancesBehavior during bind phase
        let protocol_symbol = ProtocolSymbol::with_generics(
            name,
            full_span.clone(),
            visibility_behavior,
            type_parameters.clone(),
            where_clause,
            parent.cloned(),
        );
        let protocol_arc = Arc::new(protocol_symbol);

        let protocol_type = Ty::protocol(protocol_arc.clone(), full_span.clone());
        let typed_behavior = TypedBehavior::new(protocol_type, full_span.clone());

        protocol_arc.metadata().add_behavior(typed_behavior);

        let protocol_arc_dyn = protocol_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add type parameters as children of the protocol (not the module)
        // This ensures type parameters are in scope during type resolution
        add_type_params_as_children(&type_parameters, &protocol_arc_dyn);

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&protocol_arc_dyn);
        }

        Some(protocol_arc)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        syntax: &SyntaxNode,
        context: &mut BindingContext,
    ) {
        // Only process protocol symbols
        if symbol.metadata().kind() != KestrelSymbolKind::Protocol {
            return;
        }

        let symbol_id = symbol.metadata().id();

        // Get file_id for this symbol
        let file_id = context.file_id_for_symbol(symbol).unwrap_or(context.file_id);

        // Get source for this symbol's file
        let source_file = context.source_file_name(symbol);
        let source = source_file
            .as_ref()
            .and_then(|name| context.sources.get(name))
            .map(|s| s.as_str())
            .unwrap_or("");

        // Resolve inherited protocols from syntax and add as behavior
        resolve_inherited_protocols(syntax, source, symbol, symbol_id, context, file_id);
    }
}

/// Resolve inherited protocols from syntax and add them as a ConformancesBehavior
fn resolve_inherited_protocols(
    syntax: &SyntaxNode,
    source: &str,
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) {
    // Find the ConformanceList node
    let conformance_list = match find_child(syntax, SyntaxKind::ConformanceList) {
        Some(node) => node,
        None => return,
    };

    let mut resolved_protocols = Vec::new();

    // Process each ConformanceItem
    for item in conformance_list.children() {
        if item.kind() != SyntaxKind::ConformanceItem {
            continue;
        }

        // ConformanceItem contains a Ty node
        let ty_node = match find_child(&item, SyntaxKind::Ty) {
            Some(node) => node,
            None => continue,
        };

        let span = get_node_span(&ty_node, source);

        // Find the TyPath inside Ty
        let ty_path = match ty_node.children().find(|c| c.kind() == SyntaxKind::TyPath) {
            Some(node) => node,
            None => continue,
        };

        // Extract path from TyPath
        let path_node = match find_child(&ty_path, SyntaxKind::Path) {
            Some(node) => node,
            None => continue,
        };

        // Extract path segments
        let segments: Vec<String> = path_node
            .children()
            .filter(|c| c.kind() == SyntaxKind::PathElement)
            .filter_map(|elem| {
                elem.children_with_tokens()
                    .filter_map(|e| e.into_token())
                    .find(|t| t.kind() == SyntaxKind::Identifier)
                    .map(|t| t.text().to_string())
            })
            .collect();

        if segments.is_empty() {
            continue;
        }

        let protocol_name = segments.join(".");

        // Resolve the path
        match ctx.db.resolve_type_path(segments.clone(), context_id) {
            TypePathResolution::Resolved(resolved_ty) => {
                use kestrel_semantic_tree::ty::TyKind;
                match resolved_ty.kind() {
                    TyKind::Protocol { .. } => {
                        // Valid protocol - add to resolved list
                        resolved_protocols.push(resolved_ty);
                    }
                    TyKind::Struct { symbol, .. } => {
                        ctx.diagnostics.throw(NotAProtocolError {
                            span: span.clone(),
                            name: symbol.metadata().name().value.clone(),
                            context: NotAProtocolContext::Inheritance,
                        }, file_id);
                        resolved_protocols.push(Ty::error(span));
                    }
                    _ => {
                        ctx.diagnostics.throw(NotAProtocolError {
                            span: span.clone(),
                            name: protocol_name.clone(),
                            context: NotAProtocolContext::Inheritance,
                        }, file_id);
                        resolved_protocols.push(Ty::error(span));
                    }
                }
            }
            TypePathResolution::NotFound { .. } => {
                ctx.diagnostics.throw(UnresolvedTypeError {
                    span: span.clone(),
                    type_name: protocol_name.clone(),
                }, file_id);
                resolved_protocols.push(Ty::error(span));
            }
            TypePathResolution::Ambiguous { .. } | TypePathResolution::NotAType { .. } => {
                ctx.diagnostics.throw(NotAProtocolError {
                    span: span.clone(),
                    name: protocol_name.clone(),
                    context: NotAProtocolContext::Inheritance,
                }, file_id);
                resolved_protocols.push(Ty::error(span));
            }
        }
    }

    // Add ConformancesBehavior with resolved inherited protocols
    let conformances_behavior = ConformancesBehavior::new(resolved_protocols);
    symbol.metadata().add_behavior(conformances_behavior);
}
