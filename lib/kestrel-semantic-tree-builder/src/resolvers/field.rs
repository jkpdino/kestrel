use std::sync::Arc;

use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::field::FieldSymbol;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::ty::Ty;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolver::{BindingContext, Resolver};
use crate::type_resolver::{resolve_type_with_diagnostics, TypeResolutionContext};
use crate::utils::{
    extract_name, extract_visibility, find_child, find_visibility_scope, get_node_span,
    get_visibility_span, parse_visibility,
};

/// Resolver for field declarations
pub struct FieldResolver;

impl Resolver for FieldResolver {
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

        // Check if this field is static
        let is_static = syntax
            .children()
            .any(|child| child.kind() == SyntaxKind::StaticModifier);

        // Check if this field is mutable (var vs let)
        let is_mutable = syntax
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .any(|tok| tok.kind() == SyntaxKind::Var);

        // Use error type as placeholder - actual type will be resolved in bind phase from syntax
        let field_type = Ty::error(full_span.clone());

        // Create the name object
        let name = Spanned::new(name_str, name_span);

        // Create the field symbol
        let field_symbol = FieldSymbol::new(
            name,
            full_span,
            visibility_behavior,
            is_static,
            is_mutable,
            field_type,
            parent.cloned(),
        );
        let field_arc = Arc::new(field_symbol);

        let field_arc_dyn = field_arc.clone() as Arc<dyn Symbol<KestrelLanguage>>;

        // Add to parent if exists
        if let Some(parent) = parent {
            parent.metadata().add_child(&field_arc_dyn);
        }

        Some(field_arc)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        syntax: &SyntaxNode,
        context: &mut BindingContext,
    ) {
        // Only process field symbols
        if symbol.metadata().kind() != KestrelSymbolKind::Field {
            return;
        }

        let symbol_id = symbol.metadata().id();
        let span = symbol.metadata().span().clone();

        // Get file_id for this symbol
        let file_id = context.file_id_for_symbol(symbol).unwrap_or(context.file_id);

        // Get source file name (doesn't borrow context mutably)
        let source_file = context.source_file_name(symbol);

        // Resolve the type directly from syntax
        let resolved_type = resolve_field_type_from_syntax(syntax, source_file.as_deref(), symbol_id, context, file_id);

        // Add a TypedBehavior with the resolved type
        let typed_behavior = TypedBehavior::new(resolved_type, span);
        symbol.metadata().add_behavior(typed_behavior);
    }
}

/// Resolve the field type from a FieldDeclaration syntax node
/// This extracts the type from syntax and immediately resolves it
fn resolve_field_type_from_syntax(
    syntax: &SyntaxNode,
    source_file: Option<&str>,
    context_id: semantic_tree::symbol::SymbolId,
    ctx: &mut BindingContext,
    file_id: usize,
) -> Ty {
    use crate::queries::TypePathResolution;

    // Get source from context
    let source = source_file
        .and_then(|name| ctx.sources.get(name))
        .map(|s| s.as_str())
        .unwrap_or("");

    // Find the Ty node
    if let Some(ty_node) = syntax.children().find(|child| child.kind() == SyntaxKind::Ty) {
        let ty_span = get_node_span(&ty_node, source);

        // Try to find a TyPath inside the Ty node
        if let Some(ty_path_node) = ty_node.children().find(|child| child.kind() == SyntaxKind::TyPath) {
            // Find the Path node inside TyPath
            if let Some(path_node) = ty_path_node.children().find(|child| child.kind() == SyntaxKind::Path) {
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

        // Fallback: return error type
        return Ty::error(ty_span);
    }

    // No type found - return error
    Ty::error(0..0)
}
