use std::sync::Arc;

use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::type_parameter::TypeParameterSymbol;
use kestrel_semantic_tree::ty::{Constraint, Ty, WhereClause};
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::utils::{find_child, get_node_span};

/// Extract type parameters from a syntax node that has a TypeParameterList child.
///
/// Returns a list of TypeParameterSymbols with their parsed defaults.
/// The parent is set to the container (struct, function, etc.) that owns these type parameters.
///
/// IMPORTANT: This function does NOT add type parameters as children of the parent.
/// The caller must do this after creating the owner symbol. Use `add_type_params_as_children`
/// to add them after the owner is created.
pub fn extract_type_parameters(
    syntax: &SyntaxNode,
    source: &str,
    parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
) -> Vec<Arc<TypeParameterSymbol>> {
    let type_param_list = match find_child(syntax, SyntaxKind::TypeParameterList) {
        Some(node) => node,
        None => return Vec::new(),
    };

    let mut type_params = Vec::new();

    for child in type_param_list.children() {
        if child.kind() == SyntaxKind::TypeParameter {
            if let Some(param) = parse_type_parameter(&child, source, parent.clone()) {
                let type_param = Arc::new(param);
                type_params.push(type_param);
            }
        }
    }

    type_params
}

/// Add type parameters as children of their owner symbol.
///
/// This must be called after the owner (struct, function, protocol, etc.) is created,
/// passing the owner as the parent. This ensures type parameters are in scope during
/// type resolution within the owner's body.
pub fn add_type_params_as_children(
    type_params: &[Arc<TypeParameterSymbol>],
    owner: &Arc<dyn Symbol<KestrelLanguage>>,
) {
    for type_param in type_params {
        owner.metadata().add_child(&(type_param.clone() as Arc<dyn Symbol<KestrelLanguage>>));
    }
}

/// Parse a single TypeParameter syntax node.
///
/// TypeParameter syntax:
///   - `T` (simple type parameter)
///   - `T = Int` (type parameter with default)
fn parse_type_parameter(
    syntax: &SyntaxNode,
    source: &str,
    parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
) -> Option<TypeParameterSymbol> {
    // Find the name (Identifier token directly in TypeParameter, or in Name node)
    let (name_text, name_span) = extract_type_param_name(syntax, source)?;
    let full_span = get_node_span(syntax, source);
    let name = Spanned::new(name_text, name_span);

    // Check for default type (DefaultType node)
    let default_ty = extract_default_type(syntax, source);

    let symbol = if let Some(default) = default_ty {
        TypeParameterSymbol::with_default(name, full_span, default, parent)
    } else {
        TypeParameterSymbol::new(name, full_span, parent)
    };

    Some(symbol)
}

/// Extract the name from a TypeParameter node.
fn extract_type_param_name(syntax: &SyntaxNode, _source: &str) -> Option<(String, kestrel_span::Span)> {
    // Look for direct Identifier token
    for child in syntax.children_with_tokens() {
        if let Some(token) = child.into_token() {
            if token.kind() == SyntaxKind::Identifier {
                let text_range = token.text_range();
                let span: kestrel_span::Span = (text_range.start().into())..(text_range.end().into());
                return Some((token.text().to_string(), span));
            }
        }
    }

    // Fallback: Look in Name node
    if let Some(name_node) = find_child(syntax, SyntaxKind::Name) {
        for child in name_node.children_with_tokens() {
            if let Some(token) = child.into_token() {
                if token.kind() == SyntaxKind::Identifier {
                    let text_range = token.text_range();
                    let span: kestrel_span::Span = (text_range.start().into())..(text_range.end().into());
                    return Some((token.text().to_string(), span));
                }
            }
        }
    }

    None
}

/// Extract default type from a TypeParameter node if present.
fn extract_default_type(syntax: &SyntaxNode, source: &str) -> Option<Ty> {
    let default_node = find_child(syntax, SyntaxKind::DefaultType)?;

    // The DefaultType node should contain a Ty node
    let ty_node = find_child(&default_node, SyntaxKind::Ty)?;

    // Extract type from the Ty node
    extract_ty_from_node(&ty_node, source)
}

/// Extract a Ty from a Ty syntax node.
/// This is a simplified version - for now it handles path types.
/// Full type extraction with generics will be expanded in type resolution.
fn extract_ty_from_node(ty_node: &SyntaxNode, source: &str) -> Option<Ty> {
    let span = get_node_span(ty_node, source);

    // Find the type variant node
    let variant_node = ty_node.children().next()?;

    match variant_node.kind() {
        SyntaxKind::TyUnit => Some(Ty::unit(span)),
        SyntaxKind::TyNever => Some(Ty::never(span)),
        SyntaxKind::TyPath => {
            // Extract path segments - use error as placeholder, will be resolved during bind
            let path_node = find_child(&variant_node, SyntaxKind::Path)?;
            let segments = extract_path_segments(&path_node);
            if segments.is_empty() {
                None
            } else {
                Some(Ty::error(span))
            }
        }
        SyntaxKind::TyTuple => {
            // Extract tuple elements
            let elements: Vec<Ty> = variant_node
                .children()
                .filter(|c| c.kind() == SyntaxKind::Ty)
                .filter_map(|c| extract_ty_from_node(&c, source))
                .collect();
            Some(Ty::tuple(elements, span))
        }
        _ => None,
    }
}

/// Extract path segments from a Path node.
fn extract_path_segments(path_node: &SyntaxNode) -> Vec<String> {
    path_node
        .children()
        .filter(|c| c.kind() == SyntaxKind::PathElement)
        .filter_map(|elem| {
            elem.children_with_tokens()
                .filter_map(|e| e.into_token())
                .find(|t| t.kind() == SyntaxKind::Identifier)
                .map(|t| t.text().to_string())
        })
        .collect()
}

/// Extract where clause constraints from a syntax node that has a WhereClause child.
///
/// The `type_params` map is used to look up the SymbolId for each type parameter name.
pub fn extract_where_clause(
    syntax: &SyntaxNode,
    source: &str,
    type_params: &[Arc<TypeParameterSymbol>],
) -> WhereClause {
    let where_clause_node = match find_child(syntax, SyntaxKind::WhereClause) {
        Some(node) => node,
        None => return WhereClause::new(),
    };

    let mut constraints = Vec::new();

    // Parse TypeBound nodes
    for child in where_clause_node.children() {
        if child.kind() == SyntaxKind::TypeBound {
            if let Some(constraint) = parse_type_bound(&child, source, type_params) {
                constraints.push(constraint);
            }
        }
    }

    WhereClause::with_constraints(constraints)
}

/// Parse a TypeBound syntax node.
///
/// TypeBound syntax: `T: Protocol and Protocol2`
///
/// The structure in the syntax tree is:
/// ```text
/// TypeBound
///   Name
///     Identifier "T"
///   Path
///     PathElement
///       Identifier "Protocol"
///   Path
///     PathElement
///       Identifier "Protocol2"
/// ```
///
/// If the type parameter name is not found in `type_params`, this creates an
/// unresolved constraint that will be caught by the validation pass.
fn parse_type_bound(
    syntax: &SyntaxNode,
    source: &str,
    type_params: &[Arc<TypeParameterSymbol>],
) -> Option<Constraint> {
    // Find the Name node and extract the type parameter name and span
    let name_node = find_child(syntax, SyntaxKind::Name)?;
    let name_token = name_node
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Identifier)?;

    let param_name = name_token.text().to_string();
    let text_range = name_token.text_range();
    let param_span: kestrel_span::Span = (text_range.start().into())..(text_range.end().into());

    // Look up the type parameter (may be None if undeclared)
    let param_id = type_params
        .iter()
        .find(|p| p.metadata().name().value == param_name)
        .map(|p| p.metadata().id());

    // Extract bounds (Path nodes after the Name)
    let bounds: Vec<Ty> = syntax
        .children()
        .filter(|c| c.kind() == SyntaxKind::Path)
        .map(|path_node| {
            let span = get_node_span(&path_node, source);
            // Use error as placeholder - will be resolved during bind
            Ty::error(span)
        })
        .collect();

    if bounds.is_empty() {
        None
    } else {
        // Create resolved or unresolved constraint based on whether param was found
        match param_id {
            Some(id) => Some(Constraint::type_bound(id, param_name, param_span, bounds)),
            None => Some(Constraint::unresolved_type_bound(param_name, param_span, bounds)),
        }
    }
}

/// Extract conformances from a syntax node that has a ConformanceList child.
///
/// Returns a list of Ty representing the protocols to conform to or inherit from.
/// These are unresolved path types that will be resolved during type resolution.
#[allow(dead_code)]
pub fn extract_conformances(syntax: &SyntaxNode, source: &str) -> Vec<Ty> {
    let conformance_list = match find_child(syntax, SyntaxKind::ConformanceList) {
        Some(node) => node,
        None => return Vec::new(),
    };

    conformance_list
        .children()
        .filter(|c| c.kind() == SyntaxKind::ConformanceItem)
        .filter_map(|item| {
            // ConformanceItem contains a Ty node
            let ty_node = find_child(&item, SyntaxKind::Ty)?;
            extract_ty_from_node(&ty_node, source)
        })
        .collect()
}

// Tests are in lib/kestrel-test-suite/tests/generics/ since they require
// integration with the full semantic tree building pipeline.
