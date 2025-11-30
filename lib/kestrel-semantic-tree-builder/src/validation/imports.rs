//! Validation pass for import declarations
//!
//! This pass validates that:
//! - Module paths resolve to actual modules
//! - Imported items exist in the target module
//! - Imported items are visible from the importing scope
//! - No duplicate imports create name conflicts
//!
//! Note: The target_id resolution happens in ImportResolver.bind_declaration(),
//! but all validation (error checking) happens here to maintain separation of concerns.

use std::collections::HashMap;
use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::error::*;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;
use crate::queries;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass for import declarations
pub struct ImportValidationPass;

impl ImportValidationPass {
    const NAME: &'static str = "imports";
}

impl ValidationPass for ImportValidationPass {
    fn name(&self) -> &'static str {
        Self::NAME
    }

    fn validate(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        _config: &ValidationConfig,
    ) {
        // Walk the tree and validate all import symbols
        validate_imports_in_tree(root, db, diagnostics);
    }
}

/// Recursively walk the tree and validate import symbols
fn validate_imports_in_tree(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    // Check if this is a scope that contains imports (Module or SourceFile)
    match symbol.metadata().kind() {
        KestrelSymbolKind::Module | KestrelSymbolKind::SourceFile => {
            validate_imports_in_scope(symbol, db, diagnostics);
        }
        _ => {}
    }

    // Recurse into children
    for child in symbol.metadata().children() {
        validate_imports_in_tree(&child, db, diagnostics);
    }
}

/// Validate all imports in a given scope
fn validate_imports_in_scope(
    scope_symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    let scope_id = scope_symbol.metadata().id();

    // Get all import symbols in this scope
    let import_symbols: Vec<_> = scope_symbol
        .metadata()
        .children()
        .into_iter()
        .filter(|child| matches!(child.metadata().kind(), KestrelSymbolKind::Import))
        .collect();

    // Validate each import
    for import_symbol in &import_symbols {
        validate_import(import_symbol, scope_id, db, diagnostics);
    }

    // Check for import conflicts (whole-module imports)
    check_import_conflicts(&import_symbols, scope_symbol, db, diagnostics);
}

/// Validate a single import symbol
fn validate_import(
    import_symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    scope_id: semantic_tree::symbol::SymbolId,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    // Get the import data from behavior
    let import_data = match queries::get_import_data(import_symbol) {
        Some(data) => data,
        None => {
            // Missing import data - this shouldn't happen, but don't crash
            return;
        }
    };

    let import_id = import_symbol.metadata().id();
    let file_id = get_file_id_for_symbol(import_symbol, diagnostics);

    // 1. Validate module path resolution
    let module_id = match queries::resolve_module_path(
        db,
        import_data.module_path().to_vec(),
        import_id,
    ) {
        Ok(id) => id,
        Err(mut err) => {
            // Fix up the spans in the error using the import data
            let segments = import_data.module_path_segments();
            err.path_span = import_data.module_path_span().clone();
            if err.failed_segment_index < segments.len() {
                err.failed_segment_span = segments[err.failed_segment_index].1.clone();
            }
            diagnostics.throw(err, file_id);
            return;
        }
    };

    // Get the module symbol
    let module_symbol = match db.symbol_by_id(module_id) {
        Some(s) => s,
        None => return,
    };

    // 2. Validate import items if present
    if !import_data.items().is_empty() {
        // import A.B.C.(D, E)
        for item in import_data.items() {
            // Find the symbol in the module's visible children
            let target = module_symbol
                .metadata()
                .visible_children()
                .into_iter()
                .find(|child| child.metadata().name().value == item.name);

            match target {
                Some(target_symbol) => {
                    let target_id = target_symbol.metadata().id();

                    // Check visibility using query
                    if !queries::is_visible_from(db, target_id, import_id) {
                        // Get the actual visibility from the target symbol
                        let (visibility_str, _decl_span) = get_visibility_info(&target_symbol);

                        // Get cross-file diagnostic info
                        let declaration_file_id = get_file_id_for_symbol(&target_symbol, diagnostics);
                        // Point to the target's name identifier, not the whole declaration
                        let declaration_span = Some(target_symbol.metadata().name().span.clone());

                        diagnostics.throw(
                            SymbolNotVisibleError {
                                symbol_name: item.name.clone(),
                                visibility: visibility_str,
                                import_span: item.span.clone(), // Point to the specific item
                                declaration_span,
                                declaration_file_id: Some(declaration_file_id),
                            },
                            file_id,
                        );
                    }
                }
                None => {
                    diagnostics.throw(
                        SymbolNotFoundInModuleError {
                            symbol_name: item.name.clone(),
                            module_path: import_data.module_path().to_vec(),
                            symbol_span: item.span.clone(), // Point to the specific item
                            module_span: import_symbol.metadata().span(),
                        },
                        file_id,
                    );
                }
            }
        }
    }
    // Note: Whole-module import conflicts are checked in check_import_conflicts()
}

/// Check for conflicts when doing whole-module imports
fn check_import_conflicts(
    import_symbols: &[Arc<dyn Symbol<KestrelLanguage>>],
    parent: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    // Build a map of all names that are imported or declared
    let mut name_sources: HashMap<String, Vec<NameSource>> = HashMap::new();

    // First, collect all specific imports and local declarations
    for child in parent.metadata().children() {
        let file_id = get_file_id_for_symbol(&child, diagnostics);

        match child.metadata().kind() {
            KestrelSymbolKind::Import => {
                if let Some(import_data) = queries::get_import_data(&child) {
                    // For specific imports, add each item
                    for item in import_data.items() {
                        let name = item.alias.clone().unwrap_or_else(|| item.name.clone());

                        // Check for duplicates
                        if let Some(existing_sources) = name_sources.get(&name) {
                            // Report error for duplicate import
                            if let Some(first) = existing_sources.first() {
                                diagnostics.throw(
                                    ImportConflictError {
                                        name: name.clone(),
                                        import_span: item.span.clone(),
                                        existing_span: first.span.clone(),
                                        existing_is_import: first.is_import,
                                    },
                                    file_id,
                                );
                            }
                        }

                        name_sources
                            .entry(name)
                            .or_insert_with(Vec::new)
                            .push(NameSource {
                                span: item.span.clone(),
                                file_id,
                                is_import: true,
                            });
                    }
                }
            }
            KestrelSymbolKind::Struct
            | KestrelSymbolKind::Protocol
            | KestrelSymbolKind::TypeAlias
            | KestrelSymbolKind::Function => {
                // Local declaration
                let name = child.metadata().name().value.clone();
                let name_span = child.metadata().name().span.clone();
                name_sources
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push(NameSource {
                        span: name_span,
                        file_id,
                        is_import: false,
                    });
            }
            _ => {}
        }
    }

    // Now check whole-module imports for conflicts
    for import_symbol in import_symbols {
        let import_data = match queries::get_import_data(import_symbol) {
            Some(data) => data,
            None => continue,
        };

        // Only check whole-module imports without alias
        if !import_data.items().is_empty() || import_data.alias().is_some() {
            continue;
        }

        let import_id = import_symbol.metadata().id();
        let import_file_id = get_file_id_for_symbol(import_symbol, diagnostics);

        // Resolve the module
        let module_id = match queries::resolve_module_path(
            db,
            import_data.module_path().to_vec(),
            import_id,
        ) {
            Ok(id) => id,
            Err(_) => continue, // Already reported in validate_import
        };

        let module_symbol = match db.symbol_by_id(module_id) {
            Some(s) => s,
            None => continue,
        };

        // Check each visible symbol from the module
        for child in module_symbol.metadata().visible_children() {
            let child_id = child.metadata().id();

            // Only check visible symbols
            if !queries::is_visible_from(db, child_id, import_id) {
                continue;
            }

            let name = child.metadata().name().value.clone();

            // Check if this name conflicts with existing names
            if let Some(sources) = name_sources.get(&name) {
                for source in sources {
                    // Report conflict
                    diagnostics.throw(
                        ImportConflictError {
                            name: name.clone(),
                            import_span: import_symbol.metadata().span(),
                            existing_span: source.span.clone(),
                            existing_is_import: source.is_import,
                        },
                        import_file_id,
                    );
                }
            }
        }
    }
}

/// Source of a name in the scope (for conflict detection)
#[derive(Debug, Clone)]
struct NameSource {
    span: kestrel_span::Span,
    file_id: usize,
    is_import: bool,
}

/// Get visibility information from a symbol for error reporting
fn get_visibility_info(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> (String, Option<kestrel_span::Span>) {
    use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
    use kestrel_semantic_tree::behavior::KestrelBehaviorKind;

    let behaviors = symbol.metadata().behaviors();
    let visibility_behavior = behaviors
        .iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::Visibility))
        .and_then(|b| b.as_ref().downcast_ref::<VisibilityBehavior>());

    match visibility_behavior {
        Some(vb) => {
            let vis_str = match vb.visibility() {
                Some(v) => v.to_string(),
                None => "internal".to_string(), // default
            };
            // Use the symbol's span as declaration location
            (vis_str, Some(symbol.metadata().span()))
        }
        None => ("internal".to_string(), Some(symbol.metadata().span())),
    }
}

/// Get the file_id for a symbol by walking up to its SourceFile parent
fn get_file_id_for_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &DiagnosticContext,
) -> usize {
    let mut current = symbol.clone();
    loop {
        if current.metadata().kind() == KestrelSymbolKind::SourceFile {
            let file_name = current.metadata().name().value.clone();
            return diagnostics.get_file_id(&file_name).unwrap_or(0);
        }
        match current.metadata().parent() {
            Some(parent) => current = parent,
            None => return 0,
        }
    }
}
