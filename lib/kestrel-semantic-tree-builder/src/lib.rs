mod resolver;
mod resolvers;
mod utils;
pub mod path_resolver;

use std::sync::Arc;

use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_span::Spanned;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder, SymbolTable};

use crate::resolver::ResolverRegistry;

/// Represents the root of a semantic tree
pub struct SemanticTree {
    root: Arc<dyn Symbol<KestrelLanguage>>,
    symbol_table: SymbolTable<KestrelLanguage>,
}

impl SemanticTree {
    /// Create a new empty semantic tree with a root symbol
    pub fn new() -> Self {
        let root: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(RootSymbol::new(0..0));
        let symbol_table = SymbolTable::new();

        SemanticTree {
            root,
            symbol_table,
        }
    }

    /// Get the root symbol
    pub fn root(&self) -> &Arc<dyn Symbol<KestrelLanguage>> {
        &self.root
    }

    /// Get the symbol table
    pub fn symbol_table(&self) -> &SymbolTable<KestrelLanguage> {
        &self.symbol_table
    }

    /// Get a mutable reference to the symbol table
    pub(crate) fn symbol_table_mut(&mut self) -> &mut SymbolTable<KestrelLanguage> {
        &mut self.symbol_table
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
        let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::Class) // TODO: Add RootSymbolKind
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
/// Returns: Option<(ModuleDeclaration node, path segments)>
fn validate_and_extract_module_declaration(
    syntax: &SyntaxNode,
    source: &str,
) -> Option<(SyntaxNode, Vec<String>)> {
    use kestrel_semantic_tree::symbol::module::ModuleSymbol;

    // Find all module declarations
    let module_decls: Vec<SyntaxNode> = syntax
        .children()
        .filter(|child| child.kind() == SyntaxKind::ModuleDeclaration)
        .collect();

    // Validate count
    match module_decls.len() {
        0 => {
            // No module declaration - error at first declaration or beginning of file
            let first_decl = syntax.children().next();
            let position = first_decl
                .map(|n| {
                    let start: usize = n.text_range().start().into();
                    let end: usize = n.text_range().end().into();
                    let preview_end = end.min(source.len()).min(start + 20);
                    format!("at line starting with '{}'", &source[start..preview_end])
                })
                .unwrap_or_else(|| "at beginning of file".to_string());
            panic!("No module declaration found in file. Every file must start with a module declaration (e.g., 'module MyModule') {}", position);
        }
        1 => {
            // Exactly one - check if it's first
            let module_decl = &module_decls[0];
            let first_child = syntax.children().next();

            if let Some(first) = first_child {
                if first.kind() != SyntaxKind::ModuleDeclaration {
                    panic!(
                        "Module declaration must be the first statement in the file. Found {:?} before module declaration.",
                        first.kind()
                    );
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
            // Multiple module declarations
            panic!(
                "Multiple module declarations found. Only one module declaration is allowed per file. Found {} declarations.",
                module_decls.len()
            );
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
pub fn add_file_to_tree(
    tree: &mut SemanticTree,
    file_name: &str,
    syntax: &SyntaxNode,
    source: &str,
) {
    use kestrel_semantic_tree::symbol::source_file::SourceFileSymbol;

    let registry = ResolverRegistry::new();

    // Clone root to avoid borrow conflicts
    let root = tree.root().clone();

    // Step 1: Validate and extract module declaration
    let module_decl_and_path = validate_and_extract_module_declaration(syntax, source);

    // Step 2: Build/find module hierarchy and get the module where file should be placed
    let parent_module = if let Some((_module_decl, path_segments)) = module_decl_and_path {
        build_module_hierarchy(&root, &path_segments, tree.symbol_table_mut())
    } else {
        root.clone()
    };

    // Step 3: Create a SourceFile symbol under the module
    let file_name_spanned = Spanned::new(file_name.to_string(), 0..file_name.len());
    let source_file_symbol: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(
        SourceFileSymbol::new(file_name_spanned, 0..source.len())
    );

    // Add the SourceFile symbol to the parent module
    parent_module.metadata().add_child(&source_file_symbol);

    // Add the SourceFile to the symbol table
    tree.symbol_table_mut().insert(source_file_symbol.clone());

    // Step 4: Process all top-level declarations (except module declaration)
    // They become children of the SourceFile symbol
    for child in syntax.children() {
        // Skip module declarations - they were already processed
        if child.kind() == SyntaxKind::ModuleDeclaration {
            continue;
        }

        if let Some(symbol) = walk_node(&child, source, Some(&source_file_symbol), &root, &registry) {
            // Add this symbol and all its descendants to the table
            add_symbol_to_table(&symbol, tree.symbol_table_mut());
        }
    }
}

/// Recursively add a symbol and all its children to the symbol table
fn add_symbol_to_table(symbol: &Arc<dyn Symbol<KestrelLanguage>>, table: &mut SymbolTable<KestrelLanguage>) {
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
) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
    // Look up resolver for this syntax kind
    if let Some(resolver) = registry.get(syntax.kind()) {
        // Check if terminal - if so, stop here
        if resolver.is_terminal() {
            return None;
        }

        // Resolver creates symbol and adds to parent
        if let Some(symbol) = resolver.build_declaration(syntax, source, parent, root) {
            // Walk children
            for child in syntax.children() {
                walk_node(&child, source, Some(&symbol), root, registry);
            }
            return Some(symbol);
        }
    }

    // No resolver found - walk children anyway (e.g., ClassBody)
    for child in syntax.children() {
        walk_node(&child, source, parent, root, registry);
    }

    None
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

/// Debug print a symbol and its children
fn print_symbol(symbol: &Arc<dyn Symbol<KestrelLanguage>>, level: usize) {
    let indent = "  ".repeat(level);
    let metadata = symbol.metadata();

    // Look for visibility behavior
    let visibility_str = metadata.behaviors()
        .iter()
        .find(|b| matches!(b.kind(), KestrelBehaviorKind::Visibility))
        .and_then(|b| {
            // Downcast to VisibilityBehavior
            let behavior_any = b.as_ref() as *const dyn semantic_tree::behavior::Behavior<KestrelLanguage>;
            let behavior_any = behavior_any as *const VisibilityBehavior;
            unsafe { behavior_any.as_ref() }
        })
        .and_then(|vb| vb.visibility())
        .map(|v| format!(" [{}]", v))
        .unwrap_or_default();

    println!(
        "{}{:?} '{}'{}",
        indent,
        metadata.kind(),
        metadata.name().value,
        visibility_str
    );

    // Print children
    for child in metadata.children() {
        print_symbol(&child, level + 1);
    }
}
