//! Database for semantic analysis with query caching

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use kestrel_prelude::primitives;
use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::behavior_ext::SymbolBehaviorExt;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::type_parameter::TypeParameterSymbol;
use kestrel_semantic_tree::error::ModuleNotFoundError;
use kestrel_semantic_tree::ty::{Ty, IntBits, FloatBits};
use semantic_tree::symbol::{Symbol, SymbolId};
use crate::queries::{self, Scope, Import, ImportItem, SymbolResolution, TypePathResolution, ValuePathResolution};
use crate::path_resolver;

/// Resolve a primitive type name to its semantic type
///
/// Uses kestrel-prelude constants for consistent type name handling.
fn resolve_primitive_type(name: &str, span: kestrel_span::Span) -> Option<Ty> {
    match name {
        primitives::INT => Some(Ty::int(IntBits::I64, span)),
        primitives::I8 => Some(Ty::int(IntBits::I8, span)),
        primitives::I16 => Some(Ty::int(IntBits::I16, span)),
        primitives::I32 => Some(Ty::int(IntBits::I32, span)),
        primitives::I64 => Some(Ty::int(IntBits::I64, span)),
        primitives::FLOAT => Some(Ty::float(FloatBits::F64, span)),
        primitives::F32 => Some(Ty::float(FloatBits::F32, span)),
        primitives::F64 => Some(Ty::float(FloatBits::F64, span)),
        primitives::BOOL => Some(Ty::bool(span)),
        primitives::STRING => Some(Ty::string(span)),
        primitives::SELF_TYPE => Some(Ty::self_type(span)),
        _ => None,
    }
}

/// Thread-safe registry of all symbols in the tree
#[derive(Debug, Clone)]
pub struct SymbolRegistry {
    symbols: Arc<RwLock<HashMap<SymbolId, Arc<dyn Symbol<KestrelLanguage>>>>>,
    /// Index for O(1) lookup of symbols by (kind, name)
    /// Used primarily for module path resolution
    kind_name_index: Arc<RwLock<HashMap<(KestrelSymbolKind, String), Vec<SymbolId>>>>,
}

impl SymbolRegistry {
    pub fn new() -> Self {
        Self {
            symbols: Arc::new(RwLock::new(HashMap::new())),
            kind_name_index: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a single symbol (called during build phase)
    pub fn register(&self, symbol: Arc<dyn Symbol<KestrelLanguage>>) {
        let id = symbol.metadata().id();
        let kind = symbol.metadata().kind();
        let name = symbol.metadata().name().value.clone();

        self.symbols
            .write()
            .expect("RwLock poisoned")
            .insert(id, symbol);

        // Add to kind+name index for O(1) lookups
        self.kind_name_index
            .write()
            .expect("RwLock poisoned")
            .entry((kind, name))
            .or_insert_with(Vec::new)
            .push(id);
    }

    /// Get symbol by ID
    pub fn get(&self, id: SymbolId) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        self.symbols.read().expect("RwLock poisoned").get(&id).cloned()
    }

    /// Register entire symbol tree recursively
    pub fn register_tree(&self, root: &Arc<dyn Symbol<KestrelLanguage>>) {
        self.register(root.clone());
        for child in root.metadata().children() {
            self.register_tree(&child);
        }
    }

    /// Get total number of registered symbols (for debugging)
    pub fn len(&self) -> usize {
        self.symbols.read().expect("RwLock poisoned").len()
    }

    /// Check if registry is empty
    pub fn is_empty(&self) -> bool {
        self.symbols.read().expect("RwLock poisoned").is_empty()
    }

    /// Iterate over all symbols (for module path resolution)
    pub fn iter(&self) -> impl Iterator<Item = (SymbolId, Arc<dyn Symbol<KestrelLanguage>>)> + '_ {
        SymbolRegistryIter {
            guard: self.symbols.read().expect("RwLock poisoned"),
            keys: None,
        }
    }

    /// Look up symbols by kind and name in O(1) time
    pub fn find_by_kind_and_name(
        &self,
        kind: KestrelSymbolKind,
        name: &str,
    ) -> Vec<Arc<dyn Symbol<KestrelLanguage>>> {
        let index = self.kind_name_index.read().expect("RwLock poisoned");
        let symbols = self.symbols.read().expect("RwLock poisoned");

        index
            .get(&(kind, name.to_string()))
            .map(|ids| {
                ids.iter()
                    .filter_map(|id| symbols.get(id).cloned())
                    .collect()
            })
            .unwrap_or_default()
    }
}

/// Iterator over symbol registry
struct SymbolRegistryIter<'a> {
    guard: std::sync::RwLockReadGuard<'a, HashMap<SymbolId, Arc<dyn Symbol<KestrelLanguage>>>>,
    keys: Option<Vec<SymbolId>>,
}

impl<'a> Iterator for SymbolRegistryIter<'a> {
    type Item = (SymbolId, Arc<dyn Symbol<KestrelLanguage>>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.keys.is_none() {
            self.keys = Some(self.guard.keys().copied().collect());
        }
        let keys = self.keys.as_mut()?;
        let id = keys.pop()?;
        self.guard.get(&id).map(|s| (id, s.clone()))
    }
}

impl Default for SymbolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Database for semantic queries with caching
pub struct SemanticDatabase {
    /// Symbol registry (input to queries)
    registry: SymbolRegistry,
    /// Cache for scope queries
    scope_cache: RwLock<HashMap<SymbolId, Arc<Scope>>>,
}

impl SemanticDatabase {
    /// Create a new database with the given symbol registry
    pub fn new(registry: SymbolRegistry) -> Self {
        Self {
            registry,
            scope_cache: RwLock::new(HashMap::new()),
        }
    }

    /// Get the symbol registry
    pub fn registry(&self) -> &SymbolRegistry {
        &self.registry
    }

    /// Get symbol by ID (public delegation)
    pub fn symbol_by_id(&self, id: SymbolId) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        queries::Db::symbol_by_id(self, id)
    }

    /// Get scope for a symbol (public delegation)
    pub fn scope_for(&self, symbol_id: SymbolId) -> Arc<Scope> {
        queries::Db::scope_for(self, symbol_id)
    }

    /// Get imports in scope (public delegation)
    pub fn imports_in_scope(&self, symbol_id: SymbolId) -> Vec<Arc<Import>> {
        queries::Db::imports_in_scope(self, symbol_id)
    }

    /// Check visibility (public delegation)
    pub fn is_visible_from(&self, target: SymbolId, context: SymbolId) -> bool {
        queries::Db::is_visible_from(self, target, context)
    }

    /// Resolve module path (public delegation)
    pub fn resolve_module_path(
        &self,
        path: Vec<String>,
        context: SymbolId,
    ) -> Result<SymbolId, ModuleNotFoundError> {
        queries::Db::resolve_module_path(self, path, context)
    }

    /// Compute scope for a symbol (internal implementation)
    fn compute_scope(&self, symbol_id: SymbolId) -> Arc<Scope> {
        let symbol = self.symbol_by_id(symbol_id).expect("symbol must exist");

        // Get imports using imports_in_scope query
        let imports_data = self.imports_in_scope(symbol_id);
        let mut imports = HashMap::new();

        for import in imports_data.iter() {
            // Process specific import items
            for item in &import.items {
                if let Some(target_id) = item.target_id {
                    let name = item.alias.as_ref().unwrap_or(&item.name);
                    imports
                        .entry(name.clone())
                        .or_insert_with(Vec::new)
                        .push(target_id);
                }
            }

            // Handle whole-module imports
            if import.items.is_empty() {
                if let Some(alias) = &import.alias {
                    // import A.B.C as D
                    if let Ok(module_id) = self.resolve_module_path(import.module_path.clone(), symbol_id)
                    {
                        imports
                            .entry(alias.clone())
                            .or_insert_with(Vec::new)
                            .push(module_id);
                    }
                } else {
                    // import A.B.C → import all visible symbols
                    if let Ok(module_id) = self.resolve_module_path(import.module_path.clone(), symbol_id)
                    {
                        let module_scope = self.scope_for(module_id);

                        // Import all visible declarations from module
                        for (name, ids) in &module_scope.declarations {
                            for &decl_id in ids {
                                // Check visibility
                                if self.is_visible_from(decl_id, symbol_id) {
                                    imports
                                        .entry(name.clone())
                                        .or_insert_with(Vec::new)
                                        .push(decl_id);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Get declarations (children that aren't imports)
        let declarations = symbol
            .metadata()
            .children()
            .into_iter()
            .filter(|c| !matches!(c.metadata().kind(), KestrelSymbolKind::Import))
            .fold(HashMap::new(), |mut map, child| {
                map.entry(child.metadata().name().value.clone())
                    .or_insert_with(Vec::new)
                    .push(child.metadata().id());
                map
            });

        Arc::new(Scope {
            symbol_id,
            imports,
            declarations,
            parent: symbol.metadata().parent().map(|p| p.metadata().id()),
        })
    }
}

// Implement the Db trait
impl queries::Db for SemanticDatabase {
    fn symbol_by_id(&self, id: SymbolId) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        self.registry.get(id)
    }

    fn scope_for(&self, symbol_id: SymbolId) -> Arc<Scope> {
        // Check cache first
        {
            let cache = self.scope_cache.read().expect("RwLock poisoned");
            if let Some(scope) = cache.get(&symbol_id) {
                return scope.clone();
            }
        }

        // Compute scope
        let scope = self.compute_scope(symbol_id);

        // Cache result
        {
            let mut cache = self.scope_cache.write().expect("RwLock poisoned");
            cache.insert(symbol_id, scope.clone());
        }

        scope
    }

    fn resolve_name(&self, name: String, context: SymbolId) -> SymbolResolution {
        let mut current = Some(context);

        // Walk up scope chain
        while let Some(id) = current {
            let scope = self.scope_for(id);

            // Check imports first
            if let Some(imported) = scope.imports.get(&name) {
                return if imported.len() == 1 {
                    SymbolResolution::Found(imported.clone())
                } else {
                    SymbolResolution::Ambiguous(imported.clone())
                };
            }

            // Check declarations
            if let Some(declared) = scope.declarations.get(&name) {
                return if declared.len() == 1 {
                    SymbolResolution::Found(declared.clone())
                } else {
                    SymbolResolution::Ambiguous(declared.clone())
                };
            }

            current = scope.parent;
        }

        SymbolResolution::NotFound
    }

    fn imports_in_scope(&self, symbol_id: SymbolId) -> Vec<Arc<Import>> {
        let symbol = self.symbol_by_id(symbol_id).expect("symbol must exist");

        // Find import symbols and extract their ImportDataBehavior
        symbol
            .metadata()
            .children()
            .into_iter()
            .filter(|c| matches!(c.metadata().kind(), KestrelSymbolKind::Import))
            .filter_map(|import_symbol| {
                queries::get_import_data(&import_symbol).map(|data| {
                    Arc::new(Import {
                        module_path: data.module_path().to_vec(),
                        alias: data.alias().map(|s| s.to_string()),
                        items: data
                            .items()
                            .iter()
                            .map(|i| ImportItem {
                                name: i.name.clone(),
                                alias: i.alias.clone(),
                                target_id: i.target_id,
                            })
                            .collect(),
                    })
                })
            })
            .collect()
    }

    fn is_visible_from(&self, target: SymbolId, context: SymbolId) -> bool {
        let target_symbol = self.symbol_by_id(target).expect("target symbol must exist");
        let context_symbol = self
            .symbol_by_id(context)
            .expect("context symbol must exist");

        // Use existing visibility logic
        path_resolver::is_visible_from(&target_symbol, &context_symbol)
    }

    fn resolve_module_path(
        &self,
        path: Vec<String>,
        _context: SymbolId,
    ) -> Result<SymbolId, ModuleNotFoundError> {
        if path.is_empty() {
            return Err(ModuleNotFoundError {
                path: vec![],
                failed_segment_index: 0,
                path_span: 0..0,
                failed_segment_span: 0..0,
            });
        }

        // Import paths are always absolute from the root
        // For a path like "Library", we search for a Module symbol named "Library"
        // For a path like "Library.Sub", we find "Library" then look for "Sub" in its children

        // Find first segment using O(1) index lookup
        let first_segment = &path[0];
        let modules = self.registry.find_by_kind_and_name(KestrelSymbolKind::Module, first_segment);

        let mut current = match modules.into_iter().next() {
            Some(s) => s,
            None => {
                return Err(ModuleNotFoundError {
                    path: path.clone(),
                    failed_segment_index: 0,
                    path_span: 0..0,
                    failed_segment_span: 0..0,
                });
            }
        };

        // Resolve remaining segments by searching visible children
        for (index, segment) in path.iter().enumerate().skip(1) {
            let found = current
                .metadata()
                .visible_children()
                .into_iter()
                .find(|child| child.metadata().name().value == *segment);
            match found {
                Some(child) => current = child,
                None => {
                    return Err(ModuleNotFoundError {
                        path: path.clone(),
                        failed_segment_index: index,
                        path_span: 0..0,
                        failed_segment_span: 0..0,
                    });
                }
            }
        }

        Ok(current.metadata().id())
    }

    fn resolve_type_path(&self, path: Vec<String>, context: SymbolId) -> TypePathResolution {
        if path.is_empty() {
            return TypePathResolution::NotFound {
                segment: String::new(),
                index: 0,
            };
        }

        // Handle built-in primitive types as single-segment paths
        if path.len() == 1 {
            let segment = &path[0];
            let span = 0..0; // Primitive types don't have a real span

            if let Some(ty) = resolve_primitive_type(segment, span) {
                return TypePathResolution::Resolved(ty);
            }
        }

        let context_symbol = match self.symbol_by_id(context) {
            Some(s) => s,
            None => {
                return TypePathResolution::NotFound {
                    segment: path[0].clone(),
                    index: 0,
                };
            }
        };

        // First segment: use scope-aware name resolution
        let first = &path[0];
        let first_resolution = self.resolve_name(first.clone(), context);

        let mut current_symbol = match first_resolution {
            SymbolResolution::Found(ids) if ids.len() == 1 => {
                match self.symbol_by_id(ids[0]) {
                    Some(s) => s,
                    None => {
                        return TypePathResolution::NotFound {
                            segment: first.clone(),
                            index: 0,
                        };
                    }
                }
            }
            SymbolResolution::Found(ids) => {
                // Multiple matches - ambiguous
                return TypePathResolution::Ambiguous {
                    segment: first.clone(),
                    index: 0,
                    candidates: ids,
                };
            }
            SymbolResolution::Ambiguous(ids) => {
                return TypePathResolution::Ambiguous {
                    segment: first.clone(),
                    index: 0,
                    candidates: ids,
                };
            }
            SymbolResolution::NotFound => {
                return TypePathResolution::NotFound {
                    segment: first.clone(),
                    index: 0,
                };
            }
        };

        // Subsequent segments: search visible children of the resolved symbol
        for (index, segment) in path.iter().enumerate().skip(1) {
            // Find children matching the name and visible from context
            let matches = path_resolver::find_visible_children_by_name(
                &current_symbol,
                segment,
                &context_symbol,
            );

            match matches.len() {
                0 => {
                    return TypePathResolution::NotFound {
                        segment: segment.clone(),
                        index,
                    };
                }
                1 => {
                    current_symbol = matches.into_iter().next().unwrap();
                }
                _ => {
                    return TypePathResolution::Ambiguous {
                        segment: segment.clone(),
                        index,
                        candidates: matches.iter().map(|s| s.metadata().id()).collect(),
                    };
                }
            }
        }

        // Handle TypeParameterSymbol specially - it IS a type, not something with a TypedBehavior
        if current_symbol.metadata().kind() == KestrelSymbolKind::TypeParameter {
            // Look up the symbol from registry to get the Arc
            if let Some(symbol) = self.symbol_by_id(current_symbol.metadata().id()) {
                // Downcast the Arc<dyn Symbol> to Arc<TypeParameterSymbol>
                if let Ok(type_param_arc) = symbol.into_any_arc().downcast::<TypeParameterSymbol>() {
                    let span = type_param_arc.metadata().span().clone();
                    let ty = Ty::type_parameter(type_param_arc, span);
                    return TypePathResolution::Resolved(ty);
                }
            }
        }

        // Extract type from the final symbol's TypedBehavior
        // For type aliases, we need to return the TypeAlias type (second TypedBehavior)
        // rather than the aliased type (first TypedBehavior)
        let behaviors = current_symbol.metadata().behaviors();

        // Check if this is a TypeAlias symbol by looking for a TypedBehavior with TypeAlias kind
        // Type aliases have two TypedBehaviors:
        // 1. First: the syntactic aliased type (what it points to)
        // 2. Second: the TypeAlias type (the alias itself)
        // We want the second one for type resolution so that we can detect cycles
        let typed_behaviors: Vec<_> = behaviors
            .iter()
            .filter_map(|b| {
                if matches!(b.kind(), KestrelBehaviorKind::Typed) {
                    b.as_ref().downcast_ref::<TypedBehavior>()
                } else {
                    None
                }
            })
            .collect();

        // If there are multiple TypedBehaviors, look for one with TypeAlias kind
        let type_alias_behavior = typed_behaviors.iter().find(|tb| tb.ty().is_type_alias()).copied();

        let typed_behavior = type_alias_behavior
            .or_else(|| typed_behaviors.first().copied());

        match typed_behavior {
            Some(tb) => TypePathResolution::Resolved(tb.ty().clone()),
            None => TypePathResolution::NotAType {
                symbol_id: current_symbol.metadata().id(),
            },
        }
    }

    fn resolve_value_path(&self, path: Vec<String>, context: SymbolId) -> ValuePathResolution {
        if path.is_empty() {
            return ValuePathResolution::NotFound {
                segment: String::new(),
                index: 0,
            };
        }

        let context_symbol = match self.symbol_by_id(context) {
            Some(s) => s,
            None => {
                return ValuePathResolution::NotFound {
                    segment: path[0].clone(),
                    index: 0,
                };
            }
        };

        // First segment: use scope-aware name resolution
        let first = &path[0];
        let first_resolution = self.resolve_name(first.clone(), context);

        // Handle multiple candidates for first segment (overloads)
        let first_symbols: Vec<_> = match first_resolution {
            SymbolResolution::Found(ids) => {
                ids.iter()
                    .filter_map(|id| self.symbol_by_id(*id))
                    .collect()
            }
            SymbolResolution::Ambiguous(ids) => {
                // Check if all candidates are functions (overloads)
                let symbols: Vec<_> = ids.iter()
                    .filter_map(|id| self.symbol_by_id(*id))
                    .collect();

                let all_functions = symbols.iter().all(|s| {
                    s.metadata().kind() == KestrelSymbolKind::Function
                });

                if !all_functions {
                    return ValuePathResolution::Ambiguous {
                        segment: first.clone(),
                        index: 0,
                        candidates: ids,
                    };
                }
                symbols
            }
            SymbolResolution::NotFound => {
                return ValuePathResolution::NotFound {
                    segment: first.clone(),
                    index: 0,
                };
            }
        };

        if first_symbols.is_empty() {
            return ValuePathResolution::NotFound {
                segment: first.clone(),
                index: 0,
            };
        }

        // For single-segment paths, we can resolve now
        if path.len() == 1 {
            return self.extract_value_from_symbols(&first_symbols, first, 0);
        }

        // For multi-segment paths, the first segment must resolve to a single symbol
        // (can't have overloaded modules)
        if first_symbols.len() > 1 {
            return ValuePathResolution::Ambiguous {
                segment: first.clone(),
                index: 0,
                candidates: first_symbols.iter().map(|s| s.metadata().id()).collect(),
            };
        }

        let mut current_symbol = first_symbols.into_iter().next().unwrap();

        // Subsequent segments: search visible children of the resolved symbol
        for (index, segment) in path.iter().enumerate().skip(1) {
            // Find children matching the name and visible from context
            let matches = path_resolver::find_visible_children_by_name(
                &current_symbol,
                segment,
                &context_symbol,
            );

            // If this is the last segment, handle overloads
            if index == path.len() - 1 {
                return self.extract_value_from_symbols(&matches, segment, index);
            }

            // Intermediate segments must resolve to a single symbol
            match matches.len() {
                0 => {
                    return ValuePathResolution::NotFound {
                        segment: segment.clone(),
                        index,
                    };
                }
                1 => {
                    current_symbol = matches.into_iter().next().unwrap();
                }
                _ => {
                    return ValuePathResolution::Ambiguous {
                        segment: segment.clone(),
                        index,
                        candidates: matches.iter().map(|s| s.metadata().id()).collect(),
                    };
                }
            }
        }

        // Should not reach here (handled in loop above)
        ValuePathResolution::NotFound {
            segment: path.last().cloned().unwrap_or_default(),
            index: path.len().saturating_sub(1),
        }
    }
}

impl SemanticDatabase {
    /// Helper to extract value information from a set of resolved symbols.
    /// Handles the distinction between single values and overloaded functions.
    fn extract_value_from_symbols(
        &self,
        symbols: &[Arc<dyn Symbol<KestrelLanguage>>],
        segment: &str,
        index: usize,
    ) -> ValuePathResolution {
        if symbols.is_empty() {
            return ValuePathResolution::NotFound {
                segment: segment.to_string(),
                index,
            };
        }

        // Check if all symbols are functions (potential overloads)
        let all_functions = symbols.iter().all(|s| {
            s.metadata().kind() == KestrelSymbolKind::Function
        });

        if all_functions && symbols.len() > 1 {
            // Multiple function overloads - caller must disambiguate
            return ValuePathResolution::Overloaded {
                candidates: symbols.iter().map(|s| s.metadata().id()).collect(),
            };
        }

        // Single symbol - try to extract value
        let symbol = &symbols[0];

        // First, check for ValueBehavior
        if let Some(value_beh) = symbol.value_behavior() {
            return ValuePathResolution::Symbol {
                symbol_id: symbol.metadata().id(),
                ty: value_beh.ty().clone(),
            };
        }

        // If no ValueBehavior, check for CallableBehavior (functions are values)
        if let Some(callable_beh) = symbol.callable_behavior() {
            return ValuePathResolution::Symbol {
                symbol_id: symbol.metadata().id(),
                ty: callable_beh.function_type(),
            };
        }

        // Symbol has no value behavior
        ValuePathResolution::NotAValue {
            symbol_id: symbol.metadata().id(),
        }
    }
}
