use kestrel_semantic_tree::ty::Ty;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::utils::{find_child, get_node_span};

/// Builder for creating semantic types from syntax nodes
pub struct TypeBuilder;

impl TypeBuilder {
    /// Build a semantic Type from a syntax node
    /// The node is expected to be a Ty node or a specific type variant node
    pub fn build(node: &SyntaxNode, source: &str) -> Option<Ty> {
        let span = get_node_span(node, source);

        // If it's a generic Ty wrapper, find the specific variant child
        if node.kind() == SyntaxKind::Ty {
            let child = node.children().next()?;
            return Self::build(&child, source);
        }

        match node.kind() {
            SyntaxKind::TyUnit => Some(Ty::unit(span)),
            SyntaxKind::TyNever => Some(Ty::never(span)),
            SyntaxKind::TyPath => Self::build_path(node, source),
            SyntaxKind::TyTuple => Self::build_tuple(node, source),
            SyntaxKind::TyFunction => Self::build_function(node, source),
            // Handle direct Identifier tokens (from current parser output)
            // This is a fallback for the current syntax tree structure where
            // AliasedType contains Identifier directly instead of Ty -> TyPath -> Path
            // Use error as placeholder - will be resolved during bind
            SyntaxKind::AliasedType => {
                Some(Ty::error(span))
            }
            _ => None,
        }
    }

    fn build_path(node: &SyntaxNode, source: &str) -> Option<Ty> {
        let span = get_node_span(node, source);
        // Path types are placeholders during build - actual resolution happens during bind
        // Return error type as a placeholder
        Some(Ty::error(span))
    }

    fn build_tuple(node: &SyntaxNode, source: &str) -> Option<Ty> {
        let span = get_node_span(node, source);

        let elements: Vec<Ty> = node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .filter_map(|c| Self::build(&c, source))
            .collect();

        Some(Ty::tuple(elements, span))
    }

    fn build_function(node: &SyntaxNode, source: &str) -> Option<Ty> {
        let span = get_node_span(node, source);

        // Extract parameters from TyList
        let ty_list = find_child(node, SyntaxKind::TyList)?;
        let params: Vec<Ty> = ty_list
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .filter_map(|c| Self::build(&c, source))
            .collect();

        // Extract return type - it's the Ty node that is NOT the TyList
        // Actually, looking at parser: TyFunction -> [TyList, Ty]
        // The second child (or last child) is the return type

        let return_type_node = node
            .children()
            .filter(|c| c.kind() == SyntaxKind::Ty)
            .last()?;

        let return_type = Self::build(&return_type_node, source)?;

        Some(Ty::function(params, return_type, span))
    }
}
