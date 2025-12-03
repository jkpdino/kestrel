pub mod body_resolver;
mod db;
pub mod diagnostics;
pub mod local_scope;
pub mod path_resolver;
mod queries;
mod resolver;
mod resolvers;
pub mod type_resolver;
pub mod type_syntax;
mod utils;
pub mod validation;

use std::sync::Arc;

use kestrel_prelude::primitives;
use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use kestrel_span::{Span, Spanned};
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::cycle::CycleDetector;
use semantic_tree::symbol::{Symbol, SymbolId, SymbolMetadata, SymbolMetadataBuilder, SymbolTable};

use crate::db::{SemanticDatabase, SymbolRegistry};
use crate::diagnostics::{
    DuplicateFunctionSignatureError,
    ModuleNotFirstError, MultipleModuleDeclarationsError, NoModuleDeclarationError,
};
use crate::resolver::{BindingContext, ResolverRegistry, SyntaxMap};

/// Represents the root of a semantic tree
pub struct SemanticTree {
    root: Arc<dyn Symbol<KestrelLanguage>>,
    symbol_table: SymbolTable<KestrelLanguage>,
    /// Maps symbol IDs to their original syntax nodes for the bind phase
    syntax_map: SyntaxMap,
}

impl SemanticTree {
    /// Create a new empty semantic tree with a root symbol
    pub fn new() -> Self {
        let root: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(RootSymbol::new(0..0));
        let symbol_table = SymbolTable::new();
        let syntax_map = SyntaxMap::new();

        SemanticTree { root, symbol_table, syntax_map }
    }

    /// Get the root symbol
    pub fn root(&self) -> &Arc<dyn Symbol<KestrelLanguage>> {
        &self.root
    }

    /// Get the symbol table
    ///
    /// # Deprecated
    /// This method exposes the internal symbol table which uses global name lookup.
    /// For context-aware symbol resolution that considers imports and scope,
    /// use `SemanticDatabase` and `queries::Db::resolve_type_path()` instead.
    #[deprecated(
        since = "0.1.0",
        note = "Use SemanticDatabase for context-aware symbol resolution"
    )]
    pub fn symbol_table(&self) -> &SymbolTable<KestrelLanguage> {
        &self.symbol_table
    }

    /// Get a mutable reference to the symbol table
    pub(crate) fn symbol_table_mut(&mut self) -> &mut SymbolTable<KestrelLanguage> {
        &mut self.symbol_table
    }

    /// Get the syntax map (for bind phase)
    pub(crate) fn syntax_map(&self) -> &SyntaxMap {
        &self.syntax_map
    }

    /// Get a mutable reference to the syntax map (for build phase)
    pub(crate) fn syntax_map_mut(&mut self) -> &mut SyntaxMap {
        &mut self.syntax_map
    }
}

/// Root symbol for the semantic tree
#[derive(Debug)]
struct RootSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
}

impl Symbol<KestrelLanguage> for RootSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl RootSymbol {
    fn new(source_span: std::ops::Range<usize>) -> Self {
        let name = Spanned::new("<root>".to_string(), 0..0);
        // RootSymbol uses Module kind as it represents the root of the module hierarchy
        let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::Module)
            .with_name(name)
            .with_declaration_span(0..0)
            .with_span(source_span)
            .build();

        RootSymbol { metadata }
    }
}

/// Validate module declarations in the syntax tree
///
/// According to the spec:
/// - Exactly one module declaration must be present
/// - It must be the first statement in the file
///
/// Emits diagnostics instead of panicking. Returns None if validation fails,
/// but processing can continue (declarations will be placed under root).
///
/// Returns: Option<(ModuleDeclaration node, path segments)>
fn validate_and_extract_module_declaration(
    syntax: &SyntaxNode,
    _source: &str,
    diagnostics: &mut DiagnosticContext,
    file_id: usize,
) -> Option<(SyntaxNode, Vec<String>)> {
    // Find all module declarations
    let module_decls: Vec<SyntaxNode> = syntax
        .children()
        .filter(|child| child.kind() == SyntaxKind::ModuleDeclaration)
        .collect();

    // Validate count
    match module_decls.len() {
        0 => {
            // No module declaration - emit diagnostic
            let span = if let Some(first_decl) = syntax.children().next() {
                // Point to the first declaration
                let start: usize = first_decl.text_range().start().into();
                let end: usize = first_decl.text_range().end().into();
                start..end.min(start + 1)
            } else {
                // Point to beginning of file
                0..1
            };

            let error = NoModuleDeclarationError { span };
            diagnostics.throw(error, file_id);
            None
        }
        1 => {
            // Exactly one - check if it's first
            let module_decl = &module_decls[0];
            let first_child = syntax.children().next();

            if let Some(first) = first_child {
                if first.kind() != SyntaxKind::ModuleDeclaration {
                    // Module is not first - emit diagnostic but continue processing
                    let first_start: usize = first.text_range().start().into();
                    let first_end: usize = first.text_range().end().into();
                    let module_start: usize = module_decl.text_range().start().into();
                    let module_end: usize = module_decl.text_range().end().into();

                    let error = ModuleNotFirstError {
                        module_span: module_start..module_end,
                        first_item_span: first_start..first_end,
                        first_item_kind: format!("{:?}", first.kind()),
                    };
                    diagnostics.throw(error, file_id);
                    // Continue processing despite error
                }
            }

            // Extract path segments from ModulePath child
            let module_path_node = module_decl
                .children()
                .find(|child| child.kind() == SyntaxKind::ModulePath)
                .expect("ModuleDeclaration must have ModulePath child");

            // Extract identifier tokens from ModulePath
            let path_segments: Vec<String> = module_path_node
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .filter(|tok| tok.kind() == SyntaxKind::Identifier)
                .map(|tok| tok.text().to_string())
                .collect();

            Some((module_decl.clone(), path_segments))
        }
        _ => {
            // Multiple module declarations - emit diagnostic
            let first_decl = &module_decls[0];
            let first_start: usize = first_decl.text_range().start().into();
            let first_end: usize = first_decl.text_range().end().into();

            let duplicate_spans: Vec<Span> = module_decls
                .iter()
                .skip(1)
                .map(|decl| {
                    let start: usize = decl.text_range().start().into();
                    let end: usize = decl.text_range().end().into();
                    start..end
                })
                .collect();

            let error = MultipleModuleDeclarationsError {
                first_span: first_start..first_end,
                duplicate_spans,
                count: module_decls.len(),
            };
            diagnostics.throw(error, file_id);

            // Return first module to allow partial processing
            let module_path_node = first_decl
                .children()
                .find(|child| child.kind() == SyntaxKind::ModulePath)
                .expect("ModuleDeclaration must have ModulePath child");

            let path_segments: Vec<String> = module_path_node
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .filter(|tok| tok.kind() == SyntaxKind::Identifier)
                .map(|tok| tok.text().to_string())
                .collect();

            Some((first_decl.clone(), path_segments))
        }
    }
}

/// Build module hierarchy from path segments
///
/// For a path like ["Math", "Vector"], this creates:
///   Root -> Math -> Vector
/// and returns the Vector module as the effective root.
///
/// Modules are created with Public visibility and added to the symbol table.
fn build_module_hierarchy(
    root: &Arc<dyn Symbol<KestrelLanguage>>,
    path_segments: &[String],
    table: &mut SymbolTable<KestrelLanguage>,
) -> Arc<dyn Symbol<KestrelLanguage>> {
    use kestrel_semantic_tree::behavior::visibility::{Visibility, VisibilityBehavior};
    use kestrel_semantic_tree::symbol::module::ModuleSymbol;
    use kestrel_span::Spanned;

    let mut current_parent = root.clone();

    for segment in path_segments {
        // Check if module already exists as a child
        let existing_module = current_parent
            .metadata()
            .children()
            .iter()
            .find(|child| {
                child.metadata().kind() == KestrelSymbolKind::Module
                    && child.metadata().name().value == *segment
            })
            .cloned();

        let module_symbol = if let Some(existing) = existing_module {
            // Module already exists, use it
            existing
        } else {
            // Create new module
            let name = Spanned::new(segment.clone(), 0..segment.len());
            let span = 0..segment.len(); // Placeholder span
            let visibility = VisibilityBehavior::new(Some(Visibility::Public), 0..6, root.clone());

            let module = ModuleSymbol::new(name, span, visibility);
            let module_arc: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(module);

            // Add to parent
            current_parent.metadata().add_child(&module_arc);

            // Add to symbol table
            table.insert(module_arc.clone());

            module_arc
        };

        // Move down the hierarchy
        current_parent = module_symbol;
    }

    current_parent
}

/// Add a source file to an existing semantic tree
///
/// This processes a single source file and adds its symbols to the tree.
/// The file's declarations are placed under a SourceFile symbol within the module hierarchy.
///
/// If module validation fails, diagnostics are emitted but processing continues with
/// declarations placed directly under the root.
pub fn add_file_to_tree(
    tree: &mut SemanticTree,
    file_name: &str,
    syntax: &SyntaxNode,
    source: &str,
    diagnostics: &mut DiagnosticContext,
    file_id: usize,
) {
    use kestrel_semantic_tree::symbol::source_file::SourceFileSymbol;

    let registry = ResolverRegistry::new();

    // Clone root to avoid borrow conflicts
    let root = tree.root().clone();

    // Step 1: Validate and extract module declaration (emits diagnostics on error)
    let module_decl_and_path =
        validate_and_extract_module_declaration(syntax, source, diagnostics, file_id);

    // Step 2: Build/find module hierarchy and get the module where file should be placed
    // If validation failed (None), place declarations directly under root
    let parent_module = if let Some((_module_decl, path_segments)) = module_decl_and_path {
        build_module_hierarchy(&root, &path_segments, tree.symbol_table_mut())
    } else {
        root.clone()
    };

    // Step 3: Create a SourceFile symbol under the module
    let file_name_spanned = Spanned::new(file_name.to_string(), 0..file_name.len());
    let source_file_symbol: Arc<dyn Symbol<KestrelLanguage>> =
        Arc::new(SourceFileSymbol::new(file_name_spanned, 0..source.len()));

    // Add the SourceFile symbol to the parent module
    parent_module.metadata().add_child(&source_file_symbol);

    // Add the SourceFile to the symbol table
    tree.symbol_table_mut().insert(source_file_symbol.clone());

    // Step 4: Process all top-level declarations (except module declaration)
    // They become children of the SourceFile symbol
    // Collect symbols first to avoid borrow conflicts
    let mut created_symbols = Vec::new();
    for child in syntax.children() {
        // Skip module declarations - they were already processed
        if child.kind() == SyntaxKind::ModuleDeclaration {
            continue;
        }

        if let Some(symbol) = walk_node(&child, source, Some(&source_file_symbol), &root, &registry, tree.syntax_map_mut())
        {
            created_symbols.push(symbol);
        }
    }

    // Add all created symbols to the symbol table
    for symbol in created_symbols {
        add_symbol_to_table(&symbol, tree.symbol_table_mut());
    }
}

/// Recursively add a symbol and all its children to the symbol table
fn add_symbol_to_table(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    table: &mut SymbolTable<KestrelLanguage>,
) {
    // Add this symbol
    table.insert(symbol.clone());

    // Recursively add all children
    for child in symbol.metadata().children() {
        add_symbol_to_table(&child, table);
    }
}

/// Walk a syntax node and build symbols using the resolver registry
/// Returns the created symbol, or None if the node doesn't produce a symbol or is terminal
fn walk_node(
    syntax: &SyntaxNode,
    source: &str,
    parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
    root: &Arc<dyn Symbol<KestrelLanguage>>,
    registry: &ResolverRegistry,
    syntax_map: &mut SyntaxMap,
) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
    // Look up resolver for this syntax kind
    if let Some(resolver) = registry.get(syntax.kind()) {
        // Resolver creates symbol and adds to parent
        if let Some(symbol) = resolver.build_declaration(syntax, source, parent, root) {
            // Store the syntax node for the bind phase
            syntax_map.insert(symbol.metadata().id(), syntax.clone());

            // Check if terminal - if so, don't walk children (but still return the symbol)
            if !resolver.is_terminal() {
                // Walk children
                for child in syntax.children() {
                    walk_node(&child, source, Some(&symbol), root, registry, syntax_map);
                }
            }
            return Some(symbol);
        }
    }

    // No resolver found - walk children anyway (e.g., ClassBody)
    for child in syntax.children() {
        walk_node(&child, source, parent, root, registry, syntax_map);
    }

    None
}

/// Run the binding phase on the semantic tree
///
/// This resolves all imports and checks visibility. Should be called after
/// all files have been added to the tree.
///
/// Optionally accepts a `ValidationConfig` to control which validation passes run.
/// If `None`, all passes run with default configuration.
pub fn bind_tree(
    tree: &SemanticTree,
    diagnostics: &mut DiagnosticContext,
    _file_id: usize,
) {
    bind_tree_with_config(tree, diagnostics, _file_id, None);
}

/// Run validation passes on the semantic tree
pub fn run_validation(tree: &SemanticTree, diagnostics: &mut DiagnosticContext) {
    run_validation_with_config(tree, diagnostics, None);
}

/// Run validation passes with explicit configuration
pub fn run_validation_with_config(
    tree: &SemanticTree,
    diagnostics: &mut DiagnosticContext,
    config: Option<&validation::ValidationConfig>,
) {
    let registry = SymbolRegistry::new();
    registry.register_tree(tree.root());
    let db = SemanticDatabase::new(registry);

    let validation_config = config.cloned().unwrap_or_default();
    let runner = validation::ValidationRunner::new();
    runner.run(tree.root(), &db, diagnostics, &validation_config);
}

/// Run the binding phase with explicit validation configuration
pub fn bind_tree_with_config(
    tree: &SemanticTree,
    diagnostics: &mut DiagnosticContext,
    _file_id: usize,
    config: Option<&validation::ValidationConfig>,
) {
    // Build the symbol registry from the tree
    let registry = SymbolRegistry::new();
    registry.register_tree(tree.root());

    // Create the database
    let db = SemanticDatabase::new(registry);

    // Get the resolver registry to find resolvers
    let resolver_registry = ResolverRegistry::new();

    // Create cycle detector for type alias resolution
    let mut type_alias_cycle_detector: CycleDetector<SymbolId> = CycleDetector::new();

    // Empty maps for now - in the future, these would be populated during build phase
    // TODO: Populate function_bodies during build phase for body resolution
    let function_bodies = resolver::FunctionBodyMap::new();
    let sources = resolver::SourceMap::new();

    // Walk all symbols and call bind_declaration
    // Note: file_id is determined per-symbol based on parent SourceFile
    bind_symbol(
        tree.root(),
        &db,
        diagnostics,
        &resolver_registry,
        0,
        &mut type_alias_cycle_detector,
        &function_bodies,
        &sources,
        tree.syntax_map(),
    );

    // Post-binding pass: detect duplicate function signatures
    // This catches overloads with identical signatures which are errors
    check_duplicate_signatures(tree.root(), diagnostics);

    // Run validation passes
    let validation_config = config.cloned().unwrap_or_default();
    let runner = validation::ValidationRunner::new();
    runner.run(tree.root(), &db, diagnostics, &validation_config);
}

/// Check for duplicate function signatures within each scope.
///
/// This walks the symbol tree and for each scope (module, struct),
/// checks if there are multiple functions with the same signature.
fn check_duplicate_signatures(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
) {
    use kestrel_semantic_tree::behavior::callable::CallableSignature;
    use kestrel_semantic_tree::symbol::function::FunctionSymbol;
    use std::collections::HashMap;

    let kind = symbol.metadata().kind();

    // Scopes that can contain functions: Module, Struct, SourceFile
    let is_scope = matches!(
        kind,
        KestrelSymbolKind::Module
            | KestrelSymbolKind::Struct
            | KestrelSymbolKind::SourceFile
    );

    if is_scope {
        // Collect all function signatures in this scope
        let mut signatures: HashMap<CallableSignature, Vec<Arc<dyn Symbol<KestrelLanguage>>>> =
            HashMap::new();

        for child in symbol.metadata().children() {
            if child.metadata().kind() == KestrelSymbolKind::Function {
                // Get the signature from the FunctionSymbol directly
                // The callable behavior is updated with resolved types during bind phase
                if let Some(func_sym) = child.as_ref().downcast_ref::<FunctionSymbol>() {
                    let sig = func_sym.signature();
                    signatures.entry(sig).or_default().push(child.clone());
                }
            }
        }

        // Report duplicates
        for (sig, funcs) in signatures {
            if funcs.len() > 1 {
                let first = &funcs[0];
                let first_span = first.metadata().declaration_span().clone();
                let first_file_id = get_file_id_for_symbol(first, diagnostics);

                let duplicate_spans: Vec<(Span, usize)> = funcs[1..]
                    .iter()
                    .map(|f| {
                        let span = f.metadata().declaration_span().clone();
                        let file_id = get_file_id_for_symbol(f, diagnostics);
                        (span, file_id)
                    })
                    .collect();

                diagnostics.throw(DuplicateFunctionSignatureError {
                    signature: sig.display(),
                    first_span,
                    first_file_id,
                    duplicate_spans,
                }, first_file_id);
            }
        }
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        check_duplicate_signatures(&child, diagnostics);
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

/// Recursively bind a symbol and its children
fn bind_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
    registry: &ResolverRegistry,
    current_file_id: usize,
    type_alias_cycle_detector: &mut CycleDetector<SymbolId>,
    function_bodies: &resolver::FunctionBodyMap,
    sources: &resolver::SourceMap,
    syntax_map: &SyntaxMap,
) {
    // Find resolver for this symbol kind and call bind_declaration
    let kind = symbol.metadata().kind();

    // Track file_id - when we enter a SourceFile, update the file_id
    let file_id = if kind == KestrelSymbolKind::SourceFile {
        // Look up file_id based on source file name
        let file_name = symbol.metadata().name().value.clone();
        diagnostics.get_file_id(&file_name).unwrap_or(current_file_id)
    } else {
        current_file_id
    };

    // Map symbol kind to syntax kind for resolver lookup
    let syntax_kind = match kind {
        KestrelSymbolKind::Import => Some(SyntaxKind::ImportDeclaration),
        KestrelSymbolKind::Protocol => Some(SyntaxKind::ProtocolDeclaration),
        KestrelSymbolKind::Struct => Some(SyntaxKind::StructDeclaration),
        KestrelSymbolKind::Field => Some(SyntaxKind::FieldDeclaration),
        KestrelSymbolKind::Function => Some(SyntaxKind::FunctionDeclaration),
        KestrelSymbolKind::Module => Some(SyntaxKind::ModuleDeclaration),
        KestrelSymbolKind::TypeAlias => Some(SyntaxKind::TypeAliasDeclaration),
        KestrelSymbolKind::TypeParameter => Some(SyntaxKind::TypeParameter),
        KestrelSymbolKind::SourceFile => None,
    };

    if let Some(sk) = syntax_kind {
        if let Some(resolver) = registry.get(sk) {
            // Retrieve the syntax node for this symbol from the syntax map
            if let Some(syntax_node) = syntax_map.get(&symbol.metadata().id()) {
                let mut ctx = BindingContext {
                    db,
                    diagnostics,
                    file_id,
                    type_alias_cycle_detector,
                    function_bodies,
                    sources,
                };
                resolver.bind_declaration(symbol, syntax_node, &mut ctx);
            }
        }
    }

    // Recursively bind children
    for child in symbol.metadata().children() {
        bind_symbol(&child, db, diagnostics, registry, file_id, type_alias_cycle_detector, function_bodies, sources, syntax_map);
    }
}

/// Print the semantic tree (shows symbol hierarchy)
pub fn print_semantic_tree(tree: &SemanticTree) {
    let root = tree.root();
    let children = root.metadata().children();

    println!("{} top-level symbols\n", children.len());

    // Print all children of root
    for child in children {
        print_symbol(&child, 0);
    }
}

/// Print the symbol table (shows symbols by name and kind)
#[allow(deprecated)]
pub fn print_symbol_table(tree: &SemanticTree) {
    let table = tree.symbol_table();

    println!("Symbol Table:");
    println!("  {} symbols\n", table.len());

    // Collect all symbol names and kinds for sorting
    let mut symbols: Vec<(String, String)> = Vec::new();
    for (name, collection) in table.iter() {
        for symbol in collection.multiple() {
            let kind = format!("{:?}", symbol.metadata().kind());
            symbols.push((name.clone(), kind));
        }
    }

    // Sort by name
    symbols.sort_by(|a, b| a.0.cmp(&b.0));

    // Print as a table
    println!("  {:<30} {:<15}", "Name", "Kind");
    println!("  {}", "-".repeat(45));

    for (name, kind) in symbols {
        println!("  {:<30} {:<15}", name, kind);
    }
}

/// Format a type for display
fn format_type(ty: &Ty) -> String {
    match ty.kind() {
        TyKind::Unit => "()".to_string(),
        TyKind::Never => "!".to_string(),
        TyKind::Int(bits) => format!("{:?}", bits),
        TyKind::Float(bits) => format!("{:?}", bits),
        TyKind::Bool => primitives::BOOL.to_string(),
        TyKind::String => primitives::STRING.to_string(),
        TyKind::Tuple(elements) => {
            let elem_strs: Vec<String> = elements.iter().map(format_type).collect();
            format!("({})", elem_strs.join(", "))
        }
        TyKind::Array(element_type) => {
            format!("[{}]", format_type(element_type))
        }
        TyKind::Function {
            params,
            return_type,
        } => {
            let param_strs: Vec<String> = params.iter().map(format_type).collect();
            format!(
                "({}) -> {}",
                param_strs.join(", "),
                format_type(return_type)
            )
        }
        TyKind::TypeParameter(param_symbol) => param_symbol.metadata().name().value.clone(),
        TyKind::Protocol { symbol: protocol_symbol, substitutions } => {
            let name = protocol_symbol.metadata().name().value.clone();
            if substitutions.is_empty() {
                name
            } else {
                let args: Vec<String> = substitutions.iter().map(|(_, ty)| format_type(ty)).collect();
                format!("{}[{}]", name, args.join(", "))
            }
        }
        TyKind::Struct { symbol: struct_symbol, substitutions } => {
            let name = struct_symbol.metadata().name().value.clone();
            if substitutions.is_empty() {
                name
            } else {
                let args: Vec<String> = substitutions.iter().map(|(_, ty)| format_type(ty)).collect();
                format!("{}[{}]", name, args.join(", "))
            }
        }
        TyKind::TypeAlias { symbol: type_alias_symbol, substitutions } => {
            let name = type_alias_symbol.metadata().name().value.clone();
            if substitutions.is_empty() {
                name
            } else {
                let args: Vec<String> = substitutions.iter().map(|(_, ty)| format_type(ty)).collect();
                format!("{}[{}]", name, args.join(", "))
            }
        }
        TyKind::Error => "<error>".to_string(),
        TyKind::SelfType => "Self".to_string(),
        TyKind::Inferred => "_".to_string(),
    }
}

/// Debug print a symbol and its children
fn print_symbol(symbol: &Arc<dyn Symbol<KestrelLanguage>>, level: usize) {
    let indent = "  ".repeat(level);
    let metadata = symbol.metadata();

    // Collect all behaviors
    let behaviors = metadata.behaviors();
    let behaviors_str = if !behaviors.is_empty() {
        let behavior_strings: Vec<String> = behaviors
            .iter()
            .map(|b| {
                match b.kind() {
                    KestrelBehaviorKind::Visibility => {
                        // Try to downcast to VisibilityBehavior for detailed info
                        if let Some(vb) = b.as_ref().downcast_ref::<VisibilityBehavior>() {
                            if let Some(vis) = vb.visibility() {
                                format!("Visibility({})", vis)
                            } else {
                                format!("{:?}", b.kind())
                            }
                        } else {
                            format!("{:?}", b.kind())
                        }
                    }
                    KestrelBehaviorKind::Typed => {
                        // Try to downcast to TypedBehavior for detailed info
                        if let Some(tb) = b.as_ref().downcast_ref::<TypedBehavior>() {
                            format!("Typed({})", format_type(tb.ty()))
                        } else {
                            format!("{:?}", b.kind())
                        }
                    }
                    KestrelBehaviorKind::TypeAliasTyped => {
                        format!("{:?}", b.kind())
                    }
                    KestrelBehaviorKind::ImportData => {
                        use kestrel_semantic_tree::symbol::import::ImportDataBehavior;
                        if let Some(import_data) = b.as_ref().downcast_ref::<ImportDataBehavior>() {
                            let path = import_data.module_path().join(".");
                            let items = import_data.items();
                            if items.is_empty() {
                                if let Some(alias) = import_data.alias() {
                                    format!("Import({} as {})", path, alias)
                                } else {
                                    format!("Import({})", path)
                                }
                            } else {
                                let item_strs: Vec<String> = items.iter().map(|i| {
                                    if let Some(alias) = &i.alias {
                                        format!("{} as {}", i.name, alias)
                                    } else {
                                        i.name.clone()
                                    }
                                }).collect();
                                format!("Import({}.({}))", path, item_strs.join(", "))
                            }
                        } else {
                            format!("{:?}", b.kind())
                        }
                    }
                    KestrelBehaviorKind::Callable => {
                        use kestrel_semantic_tree::behavior::callable::CallableBehavior;
                        if let Some(callable) = b.as_ref().downcast_ref::<CallableBehavior>() {
                            let params: Vec<String> = callable.parameters().iter()
                                .map(|p| {
                                    let label = p.external_label().unwrap_or("_");
                                    format!("{}: {}", label, format_type(&p.ty))
                                })
                                .collect();
                            let ret = format_type(callable.return_type());
                            format!("Callable(({}) -> {})", params.join(", "), ret)
                        } else {
                            format!("{:?}", b.kind())
                        }
                    }
                    KestrelBehaviorKind::FunctionData => {
                        use kestrel_semantic_tree::behavior::function_data::FunctionDataBehavior;
                        if let Some(fd) = b.as_ref().downcast_ref::<FunctionDataBehavior>() {
                            format!(
                                "FunctionData(has_body={}, is_static={})",
                                fd.has_body(),
                                fd.is_static()
                            )
                        } else {
                            format!("{:?}", b.kind())
                        }
                    }
                    KestrelBehaviorKind::Valued => {
                        use kestrel_semantic_tree::behavior::valued::ValueBehavior;
                        if let Some(vb) = b.as_ref().downcast_ref::<ValueBehavior>() {
                            format!("Valued({})", format_type(vb.ty()))
                        } else {
                            format!("{:?}", b.kind())
                        }
                    }
                    KestrelBehaviorKind::Conformances => {
                        use kestrel_semantic_tree::behavior::conformances::ConformancesBehavior;
                        if let Some(cb) = b.as_ref().downcast_ref::<ConformancesBehavior>() {
                            let conformances: Vec<String> = cb.conformances()
                                .iter()
                                .map(|t| format_type(t))
                                .collect();
                            format!("Conformances({})", conformances.join(", "))
                        } else {
                            format!("{:?}", b.kind())
                        }
                    }
                    KestrelBehaviorKind::Executable => {
                        use kestrel_semantic_tree::behavior::executable::ExecutableBehavior;
                        if let Some(eb) = b.as_ref().downcast_ref::<ExecutableBehavior>() {
                            let stmt_count = eb.body().statements.len();
                            let has_yield = eb.body().yield_expr().is_some();
                            format!("Executable(stmts={}, has_yield={})", stmt_count, has_yield)
                        } else {
                            format!("{:?}", b.kind())
                        }
                    }
                }
            })
            .collect();
        format!(" [{}]", behavior_strings.join(", "))
    } else {
        String::new()
    };

    println!(
        "{}{:?} '{}'{}",
        indent,
        metadata.kind(),
        metadata.name().value,
        behaviors_str
    );

    // Print children
    for child in metadata.children() {
        print_symbol(&child, level + 1);
    }
}
