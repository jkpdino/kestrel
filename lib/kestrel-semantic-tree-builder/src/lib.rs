mod resolver;
mod resolvers;
mod utils;

use std::sync::Arc;

use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::behavior::visibility::VisibilityBehavior;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_span::Spanned;
use kestrel_syntax_tree::SyntaxNode;
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder, SymbolTable};

use crate::resolver::ResolverRegistry;

/// Represents the root of a semantic tree
pub struct SemanticTree {
    root: Arc<dyn Symbol<KestrelLanguage>>,
    symbol_table: SymbolTable<KestrelLanguage>,
}

impl SemanticTree {
    /// Get the root symbol
    pub fn root(&self) -> &Arc<dyn Symbol<KestrelLanguage>> {
        &self.root
    }

    /// Get the symbol table
    pub fn symbol_table(&self) -> &SymbolTable<KestrelLanguage> {
        &self.symbol_table
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

/// Build a semantic tree from a syntax tree
/// Returns a semantic tree with a root symbol containing all top-level declarations
pub fn build_semantic_tree(syntax: &SyntaxNode, source: &str) -> SemanticTree {
    let registry = ResolverRegistry::new();
    let mut table = SymbolTable::new();

    // Create root symbol
    let root: Arc<dyn Symbol<KestrelLanguage>> = Arc::new(RootSymbol::new(0..source.len()));

    // Process all top-level declarations
    for child in syntax.children() {
        if let Some(symbol) = walk_node(&child, source, Some(&root), &root, &registry) {
            // Add this symbol and all its descendants to the table
            add_symbol_to_table(&symbol, &mut table);
        }
    }

    SemanticTree {
        root,
        symbol_table: table,
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
