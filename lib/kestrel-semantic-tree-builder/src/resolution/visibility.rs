//! Visibility checking for symbols
//!
//! This module provides `VisibilityChecker` which determines whether symbols
//! are visible from a given context based on visibility modifiers.

use std::sync::Arc;

use kestrel_semantic_tree::behavior::visibility::Visibility;
use kestrel_semantic_tree::behavior_ext::SymbolBehaviorExt;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use semantic_tree::symbol::Symbol;

use crate::syntax::find_ancestor_of_kind;

/// Checks visibility of symbols from a given context
///
/// The checker is bound to a specific context symbol and can check whether
/// any target symbol is visible from that context.
///
/// # Example
///
/// ```ignore
/// let checker = VisibilityChecker::new(&context_symbol);
/// if checker.is_visible(&target_symbol) {
///     // target is accessible from context
/// }
/// ```
pub struct VisibilityChecker<'a> {
    context: &'a Arc<dyn Symbol<KestrelLanguage>>,
}

impl<'a> VisibilityChecker<'a> {
    /// Create a new visibility checker for the given context
    pub fn new(context: &'a Arc<dyn Symbol<KestrelLanguage>>) -> Self {
        Self { context }
    }

    /// Check if a symbol is visible from this context
    pub fn is_visible(&self, symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> bool {
        let Some(visibility_behavior) = symbol.visibility_behavior() else {
            // No visibility behavior means default (internal), which is always visible
            return true;
        };

        match visibility_behavior.visibility() {
            Some(Visibility::Public) => true,
            Some(Visibility::Private) => {
                let visibility_scope = visibility_behavior.visibility_scope();
                Arc::ptr_eq(self.context, visibility_scope)
                    || self.is_ancestor(visibility_scope, self.context)
            }
            Some(Visibility::Internal) => {
                let target_module = find_ancestor_of_kind(symbol, KestrelSymbolKind::Module);
                let context_module = find_ancestor_of_kind(self.context, KestrelSymbolKind::Module);

                match (target_module, context_module) {
                    (Some(t), Some(c)) => Arc::ptr_eq(&t, &c),
                    _ => true, // If we can't determine modules, default to visible
                }
            }
            Some(Visibility::Fileprivate) => {
                let visibility_scope = visibility_behavior.visibility_scope();
                Arc::ptr_eq(self.context, visibility_scope)
                    || self.is_ancestor(visibility_scope, self.context)
            }
            None => true, // Default visibility (internal) - visible everywhere
        }
    }

    /// Find children of parent that are visible from this context and match name
    pub fn find_visible_children(
        &self,
        parent: &Arc<dyn Symbol<KestrelLanguage>>,
        name: &str,
    ) -> Vec<Arc<dyn Symbol<KestrelLanguage>>> {
        parent
            .metadata()
            .visible_children()
            .into_iter()
            .filter(|c| c.metadata().name().value == name)
            .filter(|c| self.is_visible(c))
            .collect()
    }

    /// Filter a collection of symbols to only those visible from this context
    pub fn filter_visible<I>(&self, symbols: I) -> Vec<Arc<dyn Symbol<KestrelLanguage>>>
    where
        I: IntoIterator<Item = Arc<dyn Symbol<KestrelLanguage>>>,
    {
        symbols.into_iter().filter(|s| self.is_visible(s)).collect()
    }

    /// Check if potential_ancestor is an ancestor of the given symbol
    fn is_ancestor(
        &self,
        potential_ancestor: &Arc<dyn Symbol<KestrelLanguage>>,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> bool {
        let mut current = Some(symbol.clone());

        while let Some(s) = current {
            if Arc::ptr_eq(&s, potential_ancestor) {
                return true;
            }
            current = s.metadata().parent();
        }

        false
    }
}

/// Check if a symbol is visible from the given context (standalone function)
///
/// This is a convenience function that creates a temporary `VisibilityChecker`.
/// For multiple checks from the same context, prefer creating a checker directly.
pub fn is_visible_from(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> bool {
    let checker = VisibilityChecker::new(context);
    checker.is_visible(symbol)
}

/// Find visible children of a symbol that match a name and are visible from context
///
/// This is a convenience function that creates a temporary `VisibilityChecker`.
pub fn find_visible_children_by_name(
    parent: &Arc<dyn Symbol<KestrelLanguage>>,
    name: &str,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Vec<Arc<dyn Symbol<KestrelLanguage>>> {
    let checker = VisibilityChecker::new(context);
    checker.find_visible_children(parent, name)
}
