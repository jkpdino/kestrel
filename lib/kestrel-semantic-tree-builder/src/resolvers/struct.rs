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

use crate::queries::TypePathResolution;
use crate::resolver::{BindingContext, Resolver};
use crate::resolvers::type_parameter::{add_type_params_as_children, extract_type_parameters, extract_where_clause, extract_conformances};
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope, get_node_span,
    get_visibility_span, parse_visibility,
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

        // Extract conformances (protocols this struct conforms to)
        let conformances = extract_conformances(syntax, source);

        // Create the struct symbol with type parameters, where clause, and conformances
        let struct_symbol = StructSymbol::with_generics(
            name,
            full_span.clone(),
            visibility_behavior,
            type_parameters.clone(),
            where_clause,
            conformances,
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

        // Get file_id for this symbol
        let file_id = context.file_id_for_symbol(symbol).unwrap_or(context.file_id);

        // Get source for this symbol's file
        let source_file = context.source_file_name(symbol);
        let source = source_file
            .as_ref()
            .and_then(|name| context.sources.get(name))
            .map(|s| s.as_str())
            .unwrap_or("");

        // Resolve where clause bounds from syntax
        resolve_where_clause_bounds(syntax, source, symbol_id, context, file_id);

        // Resolve conformances from syntax and store them
        resolve_conformances(syntax, source, symbol, symbol_id, context, file_id);
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
            let segments: Vec<String> = child
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
                            let diagnostic = kestrel_reporting::Diagnostic::error()
                                .with_message(format!("'{}' is not a protocol", symbol.metadata().name().value))
                                .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)
                                    .with_message("struct cannot be used as a bound")]);
                            ctx.diagnostics.add_diagnostic(diagnostic);
                        }
                        TyKind::TypeAlias { symbol, .. } => {
                            let diagnostic = kestrel_reporting::Diagnostic::error()
                                .with_message(format!("'{}' is not a protocol", symbol.metadata().name().value))
                                .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)
                                    .with_message("type alias cannot be used as a bound")]);
                            ctx.diagnostics.add_diagnostic(diagnostic);
                        }
                        _ => {
                            let diagnostic = kestrel_reporting::Diagnostic::error()
                                .with_message(format!("'{}' is not a protocol", bound_name))
                                .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)
                                    .with_message("not a protocol")]);
                            ctx.diagnostics.add_diagnostic(diagnostic);
                        }
                    }
                }
                TypePathResolution::NotFound { .. } => {
                    let diagnostic = kestrel_reporting::Diagnostic::error()
                        .with_message(format!("'{}' is not a protocol", bound_name))
                        .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)
                            .with_message("not found")]);
                    ctx.diagnostics.add_diagnostic(diagnostic);
                }
                TypePathResolution::Ambiguous { .. } | TypePathResolution::NotAType { .. } => {
                    let diagnostic = kestrel_reporting::Diagnostic::error()
                        .with_message(format!("'{}' is not a protocol", bound_name))
                        .with_labels(vec![kestrel_reporting::Label::primary(file_id, span)]);
                    ctx.diagnostics.add_diagnostic(diagnostic);
                }
            }
        }
    }
}

/// Resolve conformances from syntax and store them in the symbol
fn resolve_conformances(
    syntax: &SyntaxNode,
    source: &str,
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) {
    use kestrel_semantic_tree::symbol::r#struct::StructSymbol;

    // Find the ConformanceList node
    let conformance_list = match find_child(syntax, SyntaxKind::ConformanceList) {
        Some(node) => node,
        None => return,
    };

    let mut resolved_conformances = Vec::new();

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

        let conformance_name = segments.join(".");

        // Resolve the path
        match ctx.db.resolve_type_path(segments.clone(), context_id) {
            TypePathResolution::Resolved(resolved_ty) => {
                use kestrel_semantic_tree::ty::TyKind;
                match resolved_ty.kind() {
                    TyKind::Protocol { .. } => {
                        // Valid protocol - add to resolved conformances
                        resolved_conformances.push(resolved_ty);
                    }
                    TyKind::Struct { symbol, .. } => {
                        let diagnostic = kestrel_reporting::Diagnostic::error()
                            .with_message(format!("'{}' is not a protocol", symbol.metadata().name().value))
                            .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())
                                .with_message("struct cannot be used as a conformance")]);
                        ctx.diagnostics.add_diagnostic(diagnostic);
                        // Add error type as placeholder
                        resolved_conformances.push(Ty::error(span));
                    }
                    _ => {
                        let diagnostic = kestrel_reporting::Diagnostic::error()
                            .with_message(format!("'{}' is not a protocol", conformance_name))
                            .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())
                                .with_message("not a protocol")]);
                        ctx.diagnostics.add_diagnostic(diagnostic);
                        resolved_conformances.push(Ty::error(span));
                    }
                }
            }
            TypePathResolution::NotFound { .. } => {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("cannot find type '{}' in this scope", conformance_name))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())
                        .with_message("not found")]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                resolved_conformances.push(Ty::error(span));
            }
            TypePathResolution::Ambiguous { .. } | TypePathResolution::NotAType { .. } => {
                let diagnostic = kestrel_reporting::Diagnostic::error()
                    .with_message(format!("'{}' is not a protocol", conformance_name))
                    .with_labels(vec![kestrel_reporting::Label::primary(file_id, span.clone())]);
                ctx.diagnostics.add_diagnostic(diagnostic);
                resolved_conformances.push(Ty::error(span));
            }
        }
    }

    // Update the struct symbol with resolved conformances
    if let Some(struct_sym) = symbol.as_ref().downcast_ref::<StructSymbol>() {
        struct_sym.set_conformances(resolved_conformances);
    }
}
