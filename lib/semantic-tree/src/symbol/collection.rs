use std::sync::Arc;

use crate::{language::Language, symbol::Symbol};

type BaseSymbol<L> = Arc<dyn Symbol<L>>;

/// A collection of symbols that maintains both the original set and a filtered subset.
///
/// This struct provides functionality for symbol lookup and filtering while preserving
/// the original symbol collection. It supports operations like filtering, single symbol
/// retrieval, and alternative suggestions for cases where no exact matches are found.
///
/// # Examples
///
/// ```rust,ignore
/// let collection = symbol_table.get("foo");
/// let filtered = collection.filter(|s| s.in_scope(current_scope));
/// let function_symbol = filtered.filter(|s| s.kind() == SymbolKind::Function);
/// ```
#[derive(Debug, Clone)]
pub struct SymbolCollection<L: Language> {
    original_symbols: Vec<BaseSymbol<L>>,
    filtered_symbols: Vec<BaseSymbol<L>>,
}

impl<L: Language> SymbolCollection<L> {
    /// Creates a new `SymbolCollection` with the given symbols.
    /// Both `original_symbols` and `filtered_symbols` are initialized with the same set of symbols.
    pub fn new(symbols: Vec<BaseSymbol<L>>) -> Self {
        Self {
            original_symbols: symbols.clone(),
            filtered_symbols: symbols.clone(),
        }
    }

    /// Creates a new `SymbolCollection` by filtering the original symbols using the provided predicate.
    /// The original symbols are preserved, while `filtered_symbols` contains only symbols that match the filter.
    pub fn filter(&self, filter: impl Fn(&BaseSymbol<L>) -> bool) -> Self {
        Self {
            original_symbols: self.original_symbols.clone(),
            filtered_symbols: self
                .original_symbols
                .iter()
                .filter(|s| filter(s))
                .cloned()
                .collect(),
        }
    }

    /// Returns the single filtered symbol if exactly one symbol is present, otherwise returns `None`.
    /// This is useful for cases where you expect a unique match.
    pub fn single(&self) -> Option<&BaseSymbol<L>> {
        if self.filtered_symbols.len() != 1 {
            return None;
        }

        Some(&self.filtered_symbols[0])
    }

    /// Returns a slice of all filtered symbols.
    /// This provides access to all symbols that match the current filter criteria.
    pub fn multiple(&self) -> &[BaseSymbol<L>] {
        &self.filtered_symbols
    }

    /// Returns `true` if there are no filtered symbols, `false` otherwise.
    /// This checks whether any symbols match the current filter criteria.
    pub fn is_empty(&self) -> bool {
        self.filtered_symbols.is_empty()
    }

    /// Suggests alternative symbols from the original collection that are not in the filtered set.
    /// The alternatives are sorted according to the provided sort policy.
    /// This is useful for providing suggestions when no exact matches are found.
    pub fn suggest_alternatives<F>(&self, sort_policy: F) -> Vec<BaseSymbol<L>>
    where
        F: Fn(&BaseSymbol<L>, &BaseSymbol<L>) -> std::cmp::Ordering,
    {
        let mut alternatives: Vec<BaseSymbol<L>> = self
            .original_symbols
            .iter()
            .filter(|s| !self.filtered_symbols.iter().any(|f| Arc::ptr_eq(s, f)))
            .cloned()
            .collect();

        // Sort alternatives using the provided policy
        alternatives.sort_by(sort_policy);

        alternatives
    }
}
