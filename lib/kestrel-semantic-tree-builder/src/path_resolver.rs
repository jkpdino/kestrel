use std::sync::Arc;

use kestrel_semantic_tree::behavior::visibility::{Visibility, VisibilityBehavior};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use semantic_tree::symbol::Symbol;

/// Check if a symbol is an ancestor of the context symbol
fn is_ancestor(
    potential_ancestor: &Arc<dyn Symbol<KestrelLanguage>>,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> bool {
    let mut current = Some(context.clone());

    while let Some(symbol) = current {
        if Arc::ptr_eq(&symbol, potential_ancestor) {
            return true;
        }
        current = symbol.metadata().parent();
    }

    false
}

/// Find the containing module for a symbol
///
/// Walks up the parent chain until it finds a Module symbol.
/// Returns None if no module is found (e.g., for root symbols).
fn find_containing_module(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
    let mut current = Some(symbol.clone());

    while let Some(s) = current {
        if s.metadata().kind() == KestrelSymbolKind::Module {
            return Some(s);
        }
        current = s.metadata().parent();
    }

    None
}

/// Check if a symbol is visible from the given context
///
/// Uses the visibility scope stored in VisibilityBehavior to determine if
/// the symbol is accessible from the given context.
pub fn is_visible_from(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> bool {
    // Find the visibility behavior
    let behaviors = symbol.metadata().behaviors();
    let visibility_behavior = behaviors
        .iter()
        .find(|behavior| behavior.as_ref().is::<VisibilityBehavior>())
        .and_then(|behavior| behavior.as_ref().downcast_ref::<VisibilityBehavior>());

    let Some(visibility_behavior) = visibility_behavior else {
        // No visibility behavior means default (internal), which is always visible
        return true;
    };

    // Check the visibility level
    match visibility_behavior.visibility() {
        Some(Visibility::Public) => {
            // Public symbols are visible everywhere
            return true;
        }
        Some(Visibility::Private) => {
            // Private symbols are only visible within the visibility scope
            let visibility_scope = visibility_behavior.visibility_scope();
            return Arc::ptr_eq(context, visibility_scope) || is_ancestor(visibility_scope, context);
        }
        Some(Visibility::Internal) => {
            // Internal symbols are visible within the same module
            let target_module = find_containing_module(symbol);
            let context_module = find_containing_module(context);

            // If both are in the same module (or we can't determine), allow access
            match (target_module, context_module) {
                (Some(t), Some(c)) => return Arc::ptr_eq(&t, &c),
                // If we can't determine modules, default to visible
                _ => return true,
            }
        }
        Some(Visibility::Fileprivate) => {
            // File-private symbols are visible within the same file
            // For now, treat as visible if in same SourceFile
            let visibility_scope = visibility_behavior.visibility_scope();
            return Arc::ptr_eq(context, visibility_scope) || is_ancestor(visibility_scope, context);
        }
        None => {
            // Default visibility (internal) - visible everywhere for now
            return true;
        }
    }
}

/// Find visible children of a symbol that match a name and are visible from context
///
/// This is a shared helper used by type and value path resolution to avoid
/// duplicating the filtering logic.
///
/// # Arguments
/// * `parent` - The symbol whose children to search
/// * `name` - The name to match against child symbol names
/// * `context` - The context symbol for visibility checking
///
/// # Returns
/// A vector of matching children that are visible from the context
pub fn find_visible_children_by_name(
    parent: &Arc<dyn Symbol<KestrelLanguage>>,
    name: &str,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Vec<Arc<dyn Symbol<KestrelLanguage>>> {
    parent
        .metadata()
        .visible_children()
        .into_iter()
        .filter(|c| c.metadata().name().value == name)
        .filter(|c| is_visible_from(c, context))
        .collect()
}
