//! Query system for semantic analysis and scope resolution
//!
//! This module provides a query-based API for resolving symbols and scopes.
//! Queries are memoized for performance (can be upgraded to Salsa later).

use std::collections::HashMap;
use std::sync::Arc;
use semantic_tree::symbol::SymbolId;
use kestrel_semantic_tree::error::ModuleNotFoundError;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::import::ImportDataBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use semantic_tree::symbol::Symbol;

/// Result of name resolution
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolResolution {
    /// Successfully resolved to one or more symbols
    Found(Vec<SymbolId>),
    /// Name not found in any scope
    NotFound,
    /// Name found but ambiguous (multiple candidates)
    Ambiguous(Vec<SymbolId>),
}

impl SymbolResolution {
    pub fn is_found(&self) -> bool {
        matches!(self, SymbolResolution::Found(_))
    }

    pub fn single(&self) -> Option<SymbolId> {
        match self {
            SymbolResolution::Found(ids) if ids.len() == 1 => Some(ids[0]),
            _ => None,
        }
    }
}

/// Scope information for a symbol
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    /// The symbol this scope belongs to
    pub symbol_id: SymbolId,
    /// Imported names -> symbol IDs
    pub imports: HashMap<String, Vec<SymbolId>>,
    /// Declared names -> symbol IDs
    pub declarations: HashMap<String, Vec<SymbolId>>,
    /// Parent scope for lookup chain
    pub parent: Option<SymbolId>,
}

/// Import metadata (extracted from ImportDataBehavior)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    /// The module path (e.g., ["A", "B", "C"])
    pub module_path: Vec<String>,
    /// Optional alias for the module
    pub alias: Option<String>,
    /// Specific items to import
    pub items: Vec<ImportItem>,
}

/// An individual import item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportItem {
    /// Name of the symbol to import
    pub name: String,
    /// Optional alias for this import
    pub alias: Option<String>,
    /// Resolved target symbol ID (filled during bind)
    pub target_id: Option<SymbolId>,
}

/// Database trait for semantic queries
pub trait Db {
    /// Get symbol by ID from the registry
    fn symbol_by_id(&self, id: SymbolId) -> Option<Arc<dyn Symbol<KestrelLanguage>>>;

    /// Get the scope for a symbol
    fn scope_for(&self, symbol_id: SymbolId) -> Arc<Scope>;

    /// Resolve a name in a given scope context
    fn resolve_name(&self, name: String, context: SymbolId) -> SymbolResolution;

    /// Get all imports declared in a symbol's scope
    fn imports_in_scope(&self, symbol_id: SymbolId) -> Vec<Arc<Import>>;

    /// Check if target is visible from context
    fn is_visible_from(&self, target: SymbolId, context: SymbolId) -> bool;

    /// Resolve a module path from a context
    fn resolve_module_path(
        &self,
        path: Vec<String>,
        context: SymbolId,
    ) -> Result<SymbolId, ModuleNotFoundError>;
}

/// Get the scope for a symbol
pub fn scope_for(db: &dyn Db, symbol_id: SymbolId) -> Arc<Scope> {
    db.scope_for(symbol_id)
}

/// Resolve a name in a given scope context
pub fn resolve_name(db: &dyn Db, name: String, context: SymbolId) -> SymbolResolution {
    db.resolve_name(name, context)
}

/// Get all imports declared in a symbol's scope
pub fn imports_in_scope(db: &dyn Db, symbol_id: SymbolId) -> Vec<Arc<Import>> {
    db.imports_in_scope(symbol_id)
}

/// Check if target is visible from context
pub fn is_visible_from(db: &dyn Db, target: SymbolId, context: SymbolId) -> bool {
    db.is_visible_from(target, context)
}

/// Resolve a module path from a context
pub fn resolve_module_path(
    db: &dyn Db,
    path: Vec<String>,
    context: SymbolId,
) -> Result<SymbolId, ModuleNotFoundError> {
    db.resolve_module_path(path, context)
}

/// Helper to get ImportDataBehavior from a symbol
pub fn get_import_data(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>
) -> Option<Arc<ImportDataBehavior>> {
    symbol
        .metadata()
        .behaviors()
        .into_iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::ImportData))
        .and_then(|b| {
            b.as_ref()
                .downcast_ref::<ImportDataBehavior>()
                .map(|data| Arc::new(data.clone()))
        })
}
