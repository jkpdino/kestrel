use std::sync::Arc;

use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::diagnostics::{NotAProtocolContext, NotAProtocolError, UnresolvedTypeError};
use crate::database::TypePathResolution;
use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause};
use crate::syntax::{
    extract_name, extract_path_segments, extract_visibility, find_child, find_visibility_scope,
    get_node_span, get_visibility_span, parse_visibility, resolve_conformance_list,
};

/// Resolver for struct declarations
pub struct StructResolver;

impl Resolver for StructResolver {
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

        // Extract type parameters (they'll have struct as parent later)
        let type_parameters = extract_type_parameters(syntax, source, parent.cloned());

        // Extract where clause (uses type_parameters to look up SymbolIds)
        let where_clause = extract_where_clause(syntax, source, &type_parameters);

        // Create the struct symbol with type parameters and where clause
        // Conformances are resolved and added as a behavior during bind phase
        let struct_symbol = StructSymbol::with_generics(
            name,
            full_span.clone(),
            visibility_behavior,
            type_parameters.clone(),
            where_clause,
            parent.cloned(),
        );
        let struct_arc = Arc::new(struct_symbol);

        let struct_type = Ty::r#struct(struct_arc.clone(), full_span.clone());
        let typed_behavior = TypedBehavior::new(struct_type, full_span.clone());

        struct_arc.metadata().add_behavior(typed_behavior);

        let struct_arc_dyn = struct_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add type parameters as children of the struct (not the module)
        // This ensures type parameters are in scope during type resolution
        add_type_params_as_children(&type_parameters, &struct_arc_dyn);

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&struct_arc_dyn);
        }

        Some(struct_arc)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        syntax: &SyntaxNode,
        context: &mut BindingContext,
    ) {
        // Only process struct symbols
        if symbol.metadata().kind() != KestrelSymbolKind::Struct {
            return;
        }

        let symbol_id = symbol.metadata().id();

        // Get file_id and source for this symbol
        let (file_id, source) = context.get_file_context(symbol);

        // Resolve where clause bounds from syntax
        resolve_where_clause_bounds(syntax, &source, symbol_id, context, file_id);

        // Resolve conformances from syntax and store them
        resolve_conformance_list(
            syntax,
            &source,
            symbol,
            symbol_id,
            context,
            file_id,
            NotAProtocolContext::Conformance,
        );
    }
}

/// Resolve and validate where clause bounds from syntax
fn resolve_where_clause_bounds(
    syntax: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) {
    // Find the WhereClause node
    let where_clause_node = match find_child(syntax, SyntaxKind::WhereClause) {
        Some(node) => node,
        None => return,
    };

    // Process each TypeBound node
    for child in where_clause_node.children() {
        if child.kind() == SyntaxKind::TypeBound {
            resolve_type_bound(&child, source, context_id, ctx, file_id);
        }
    }
}

/// Resolve and validate a single type bound
fn resolve_type_bound(
    syntax: &SyntaxNode,
    source: &str,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) {
    // Each Path node in a TypeBound is a bound (after the Name node)
    for child in syntax.children() {
        if child.kind() == SyntaxKind::Path {
            let span = get_node_span(&child, source);

            // Extract path segments
            let segments = extract_path_segments(&child);

            if segments.is_empty() {
                continue;
            }

            let bound_name = segments.join(".");

            // Resolve the path
            match ctx.db.resolve_type_path(segments.clone(), context_id) {
                TypePathResolution::Resolved(resolved_ty) => {
                    // Check if it's a protocol
                    use kestrel_semantic_tree::ty::TyKind;
                    match resolved_ty.kind() {
                        TyKind::Protocol { .. } => {
                            // Valid protocol bound - nothing to do
                        }
                        TyKind::Struct { symbol, .. } => {
                            ctx.diagnostics.throw(NotAProtocolError {
                                span,
                                name: symbol.metadata().name().value.clone(),
                                context: NotAProtocolContext::Bound,
                            }, file_id);
                        }
                        TyKind::TypeAlias { symbol, .. } => {
                            ctx.diagnostics.throw(NotAProtocolError {
                                span,
                                name: symbol.metadata().name().value.clone(),
                                context: NotAProtocolContext::Bound,
                            }, file_id);
                        }
                        _ => {
                            ctx.diagnostics.throw(NotAProtocolError {
                                span,
                                name: bound_name.clone(),
                                context: NotAProtocolContext::Bound,
                            }, file_id);
                        }
                    }
                }
                TypePathResolution::NotFound { .. } => {
                    ctx.diagnostics.throw(UnresolvedTypeError {
                        span,
                        type_name: bound_name.clone(),
                    }, file_id);
                }
                TypePathResolution::Ambiguous { .. } | TypePathResolution::NotAType { .. } => {
                    ctx.diagnostics.throw(NotAProtocolError {
                        span,
                        name: bound_name.clone(),
                        context: NotAProtocolContext::Bound,
                    }, file_id);
                }
            }
        }
    }
}

