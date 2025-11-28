use std::sync::Arc;

use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::import::{ImportSymbol, ImportDataBehavior, ImportItem};
use kestrel_semantic_tree::error::*;
use kestrel_span::{Span, Spanned};
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use kestrel_parser::import::ImportDeclaration;
use kestrel_parser::module::ModulePath;
use semantic_tree::symbol::Symbol;

use crate::resolver::{Resolver, BindingContext};
use crate::utils::get_node_span;
use crate::queries;

/// Resolver for import declarations
pub struct ImportResolver;

impl Resolver for ImportResolver {
    fn build_declaration(
        &self,
        syntax: &SyntaxNode,
        source: &str,
        parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        _root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        let parent = parent?;

        // Wrap syntax node in ImportDeclaration helper
        let import_decl = ImportDeclaration {
            syntax: syntax.clone(),
            span: get_node_span(syntax, source),
        };

        // Extract module path
        let module_path_node = import_decl.path();
        let module_path = extract_module_path(&module_path_node, source);

        // Extract alias
        let alias = import_decl.alias();

        // Extract import items
        let items = extract_import_items(&import_decl, source);

        // Create import symbol name
        let import_name = if let Some(ref alias) = alias {
            alias.clone()
        } else {
            module_path.join(".")
        };

        // NOTE: Span may be incorrect due to rowan position calculation issue
        // when lexer skips whitespace/comments. See utils::get_node_span for details.
        let span = get_node_span(syntax, source);
        let name = Spanned::new(import_name, span.clone());

        // Create import symbol
        let import_symbol = ImportSymbol::new(name, parent.clone(), span);
        let import_arc: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(import_symbol);

        // Store import data in behavior for bind phase
        let import_data = ImportDataBehavior::new(module_path, alias, items);
        import_arc.metadata().add_behavior(import_data);

        // Add to parent
        parent.metadata().add_child(&import_arc);

        Some(import_arc)
    }

    fn bind_declaration(
        &self,
        symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        ctx: &mut BindingContext,
    ) {
        // Get import data from behavior
        let import_data = match get_import_data_behavior(symbol) {
            Some(data) => data,
            None => {
                eprintln!("Warning: ImportSymbol missing ImportDataBehavior");
                return;
            }
        };

        let import_id = symbol.metadata().id();

        // Resolve module path using query
        let module_id = match queries::resolve_module_path(
            ctx.db,
            import_data.module_path().to_vec(),
            import_id,
        ) {
            Ok(id) => id,
            Err(err) => {
                ctx.diagnostics.throw(err, ctx.file_id);
                return;
            }
        };

        // Get the module symbol to check its children directly (avoid scope_for recursion)
        let module_symbol = match ctx.db.symbol_by_id(module_id) {
            Some(s) => s,
            None => return,
        };

        // Validate import items if present
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
                        if queries::is_visible_from(ctx.db, target_id, import_id) {
                            // Visibility check passed - record the resolved target
                            import_data.set_target_id(&item.name, target_id);
                        } else {
                            // Get the actual visibility from the target symbol
                            let (visibility_str, _decl_span) = get_visibility_info(&target_symbol);

                            // Get cross-file diagnostic info
                            let declaration_file_id = ctx.file_id_for_symbol(&target_symbol);
                            let declaration_span = Some(target_symbol.metadata().span());

                            ctx.diagnostics.throw(
                                SymbolNotVisibleError {
                                    symbol_name: item.name.clone(),
                                    visibility: visibility_str,
                                    import_span: symbol.metadata().span(),
                                    declaration_span,
                                    declaration_file_id,
                                },
                                ctx.file_id,
                            );
                        }
                    }
                    None => {
                        ctx.diagnostics.throw(
                            SymbolNotFoundInModuleError {
                                symbol_name: item.name.clone(),
                                module_path: import_data.module_path().to_vec(),
                                symbol_span: symbol.metadata().span(),
                                module_span: symbol.metadata().span(),
                            },
                            ctx.file_id,
                        );
                    }
                }
            }
        } else if import_data.alias().is_none() {
            // Whole-module import without alias: import A.B.C
            // Check for name conflicts with other imports/declarations
            check_whole_module_import_conflicts(symbol, &module_symbol, ctx);
        }
        // For import A.B.C as D, no conflict check needed (alias is unique)
    }

    fn is_terminal(&self) -> bool {
        true // Don't walk children of import declarations
    }
}

/// Extract module path from ModulePath syntax node
fn extract_module_path(module_path: &ModulePath, _source: &str) -> Vec<String> {
    module_path.segment_names()
}

/// Extract import items from import declaration
fn extract_import_items(import_decl: &ImportDeclaration, source: &str) -> Vec<ImportItem> {
    import_decl
        .items()
        .into_iter()
        .filter_map(|item_node| {
            // Get the name (first identifier)
            let name = item_node
                .children_with_tokens()
                .find_map(|elem| {
                    elem.as_token()
                        .filter(|t| t.kind() == SyntaxKind::Identifier)
                        .map(|t| t.text().to_string())
                })?;

            // Check for alias (identifier after "as" keyword)
            let mut found_as = false;
            let alias = item_node.children_with_tokens().find_map(|elem| {
                if let Some(token) = elem.as_token() {
                    if found_as && token.kind() == SyntaxKind::Identifier {
                        return Some(token.text().to_string());
                    }
                    if token.kind() == SyntaxKind::As {
                        found_as = true;
                    }
                }
                None
            });

            Some(ImportItem {
                name,
                alias,
                target_id: None, // Filled during bind phase
            })
        })
        .collect()
}

/// Get ImportDataBehavior from a symbol
fn get_import_data_behavior(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Option<Arc<ImportDataBehavior>> {
    crate::queries::get_import_data(symbol)
}

/// Get visibility information from a symbol for error reporting
fn get_visibility_info(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> (String, Option<Span>) {
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

/// Check for conflicts when doing a whole-module import.
///
/// For `import A.B.C` (without alias or specific items), all visible symbols
/// from the module are imported. This function checks if any of those names
/// conflict with:
/// 1. Symbols from other imports in the same file
/// 2. Local declarations in the same file
fn check_whole_module_import_conflicts(
    import_symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    module_symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    ctx: &mut BindingContext,
) {
    use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
    use std::collections::HashMap;

    // Get the parent (SourceFile) to check sibling imports and declarations
    let parent = match import_symbol.metadata().parent() {
        Some(p) => p,
        None => return,
    };

    let import_id = import_symbol.metadata().id();

    // Collect names from other imports and local declarations in the same file
    let mut existing_names: HashMap<String, (Span, bool)> = HashMap::new(); // name -> (span, is_import)

    for sibling in parent.metadata().children() {
        let sibling_id = sibling.metadata().id();

        // Skip self
        if sibling_id == import_id {
            continue;
        }

        match sibling.metadata().kind() {
            KestrelSymbolKind::Import => {
                // Check what this import brings in
                if let Some(import_data) = get_import_data_behavior(&sibling) {
                    // For specific imports, check the items
                    for item in import_data.items() {
                        let name = item.alias.clone().unwrap_or_else(|| item.name.clone());
                        existing_names.insert(name, (sibling.metadata().span(), true));
                    }

                    // For whole-module imports without alias, we'd need to check that too
                    // but for simplicity, we only detect conflicts with specific imports
                    // and local declarations for now
                }
            }
            KestrelSymbolKind::Class | KestrelSymbolKind::TypeAlias => {
                // Local declaration
                let name = sibling.metadata().name().value.clone();
                existing_names.insert(name, (sibling.metadata().span(), false));
            }
            _ => {}
        }
    }

    // Now check which symbols from the imported module would conflict
    let visible_children = module_symbol.metadata().visible_children();

    for child in visible_children {
        // Only check visible symbols
        let child_id = child.metadata().id();
        if !queries::is_visible_from(ctx.db, child_id, import_id) {
            continue;
        }

        let name = child.metadata().name().value.clone();

        if let Some((existing_span, is_import)) = existing_names.get(&name) {
            // Conflict found!
            ctx.diagnostics.throw(
                ImportConflictError {
                    name,
                    import_span: import_symbol.metadata().span(),
                    existing_span: existing_span.clone(),
                    existing_is_import: *is_import,
                },
                ctx.file_id,
            );
        }
    }
}
