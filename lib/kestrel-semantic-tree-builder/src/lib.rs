//! Semantic tree builder for the Kestrel language
//!
//! This crate provides the infrastructure for building semantic trees from syntax trees.
//! It handles:
//! - Building symbol hierarchies from syntax
//! - Resolving type references and imports
//! - Validating semantic constraints
//!
//! # Architecture
//!
//! The crate is organized into several modules:
//!
//! - `builder`: Tree construction from syntax (`SemanticTreeBuilder`)
//! - `database`: Query system for semantic analysis (`SemanticDatabase`, `Db` trait)
//! - `resolution`: Binding and type resolution (`SemanticBinder`, `TypeResolver`, `VisibilityChecker`)
//! - `syntax`: Syntax tree utilities
//! - `diagnostics`: Error types for semantic analysis
//! - `validation`: Validation passes
//! - `resolvers`: Per-declaration resolvers
//! - `body_resolver`: Function body resolution
//!
//! # Usage
//!
//! ```ignore
//! use kestrel_semantic_tree_builder::{SemanticTreeBuilder, SemanticBinder};
//!
//! // Build phase
//! let mut builder = SemanticTreeBuilder::new();
//! builder.add_file("main.kes", &syntax, &source, &mut diagnostics, 0);
//! let tree = builder.build();
//!
//! // Bind phase
//! let mut binder = SemanticBinder::new(&tree);
//! binder.bind(&mut diagnostics);
//! ```

// Core modules
pub mod builder;
pub mod database;
mod debug;
pub mod resolution;
mod resolver;
mod resolvers;
pub mod syntax;
mod tree;

// Feature modules
pub mod body_resolver;
pub mod diagnostics;
pub mod validation;

// Re-exports for convenient access
pub use builder::SemanticTreeBuilder;
pub use database::{Db, SemanticDatabase, SymbolRegistry};
pub use debug::{format_type, print_semantic_tree, print_symbol_table};
pub use resolution::{LocalScope, SemanticBinder, TypeResolver, VisibilityChecker};
pub use resolver::{BindingContext, Resolver, ResolverRegistry};
pub use tree::SemanticTree;

// Legacy re-exports for backwards compatibility
// These allow existing code to continue working during migration
pub use resolution::visibility::{find_visible_children_by_name, is_visible_from};
pub use syntax::{
    extract_identifier_from_name, extract_name, extract_path_segments, extract_visibility,
    find_ancestor_of_kind, find_child, find_visibility_scope, get_file_id_for_symbol,
    get_node_span, get_source_file_info, get_visibility_span, parse_visibility,
    resolve_conformance_list, SourceFileInfo,
};

// Type resolution re-exports
pub use resolution::type_resolver::{extract_type_from_node, extract_type_from_ty_node};

/// Convenience function to add a file to a tree (legacy API)
///
/// Prefer using `SemanticTreeBuilder` directly for new code.
pub fn add_file_to_tree(
    tree: &mut SemanticTree,
    file_name: &str,
    syntax: &kestrel_syntax_tree::SyntaxNode,
    source: &str,
    diagnostics: &mut kestrel_reporting::DiagnosticContext,
    file_id: usize,
) {
    // Create a temporary builder, add the file, then extract just what we need
    // This is a bit inefficient but maintains backwards compatibility
    use kestrel_semantic_tree::symbol::source_file::SourceFileSymbol;
    use kestrel_span::Spanned;
    use std::sync::Arc;

    let root = tree.root().clone();

    // Validate and extract module declaration
    let mut validator = builder::ModuleValidator::new(syntax, diagnostics, file_id);
    let module_decl = validator.validate();

    // Build/find module hierarchy
    let parent_module = if let Some(decl) = module_decl {
        tree::build_module_hierarchy(&root, &decl.path, tree.symbol_table_mut())
    } else {
        root.clone()
    };

    // Create SourceFile symbol
    let file_name_spanned = Spanned::new(file_name.to_string(), 0..file_name.len());
    let source_file_symbol: Arc<dyn semantic_tree::symbol::Symbol<kestrel_semantic_tree::language::KestrelLanguage>> =
        Arc::new(SourceFileSymbol::new(file_name_spanned, 0..source.len()));

    parent_module.metadata().add_child(&source_file_symbol);
    tree.symbol_table_mut().insert(source_file_symbol.clone());

    // Process declarations
    let resolver_registry = ResolverRegistry::new();
    let mut created_symbols = Vec::new();

    for child in syntax.children() {
        if child.kind() == kestrel_syntax_tree::SyntaxKind::ModuleDeclaration {
            continue;
        }

        if let Some(symbol) = walk_node_legacy(
            &child,
            source,
            Some(&source_file_symbol),
            &root,
            &resolver_registry,
            tree.syntax_map_mut(),
        ) {
            created_symbols.push(symbol);
        }
    }

    for symbol in created_symbols {
        add_symbol_to_table_legacy(&symbol, tree.symbol_table_mut());
    }

    tree.sources_mut().insert(file_name.to_string(), source.to_string());
}

/// Legacy helper for walking nodes
fn walk_node_legacy(
    syntax: &kestrel_syntax_tree::SyntaxNode,
    source: &str,
    parent: Option<&std::sync::Arc<dyn semantic_tree::symbol::Symbol<kestrel_semantic_tree::language::KestrelLanguage>>>,
    root: &std::sync::Arc<dyn semantic_tree::symbol::Symbol<kestrel_semantic_tree::language::KestrelLanguage>>,
    registry: &ResolverRegistry,
    syntax_map: &mut tree::SyntaxMap,
) -> Option<std::sync::Arc<dyn semantic_tree::symbol::Symbol<kestrel_semantic_tree::language::KestrelLanguage>>> {
    if let Some(resolver) = registry.get(syntax.kind()) {
        if let Some(symbol) = resolver.build_declaration(syntax, source, parent, root) {
            syntax_map.insert(symbol.metadata().id(), syntax.clone());

            if !resolver.is_terminal() {
                for child in syntax.children() {
                    walk_node_legacy(&child, source, Some(&symbol), root, registry, syntax_map);
                }
            }
            return Some(symbol);
        }
    }

    for child in syntax.children() {
        walk_node_legacy(&child, source, parent, root, registry, syntax_map);
    }

    None
}

/// Legacy helper for adding symbols to table
fn add_symbol_to_table_legacy(
    symbol: &std::sync::Arc<dyn semantic_tree::symbol::Symbol<kestrel_semantic_tree::language::KestrelLanguage>>,
    table: &mut semantic_tree::symbol::SymbolTable<kestrel_semantic_tree::language::KestrelLanguage>,
) {
    table.insert(symbol.clone());
    for child in symbol.metadata().children() {
        add_symbol_to_table_legacy(&child, table);
    }
}

/// Convenience function to bind a tree (legacy API)
///
/// Prefer using `SemanticBinder` directly for new code.
pub fn bind_tree(
    tree: &SemanticTree,
    diagnostics: &mut kestrel_reporting::DiagnosticContext,
    _file_id: usize,
) {
    bind_tree_with_config(tree, diagnostics, _file_id, None);
}

/// Convenience function to bind a tree with config (legacy API)
pub fn bind_tree_with_config(
    tree: &SemanticTree,
    diagnostics: &mut kestrel_reporting::DiagnosticContext,
    _file_id: usize,
    config: Option<&validation::ValidationConfig>,
) {
    let mut binder = SemanticBinder::new(tree);
    binder.bind_with_config(diagnostics, config);
}

/// Run validation passes on the semantic tree (legacy API)
pub fn run_validation(tree: &SemanticTree, diagnostics: &mut kestrel_reporting::DiagnosticContext) {
    run_validation_with_config(tree, diagnostics, None);
}

/// Run validation passes with explicit configuration (legacy API)
pub fn run_validation_with_config(
    tree: &SemanticTree,
    diagnostics: &mut kestrel_reporting::DiagnosticContext,
    config: Option<&validation::ValidationConfig>,
) {
    let registry = SymbolRegistry::new();
    registry.register_tree(tree.root());
    let db = SemanticDatabase::new(registry);

    let validation_config = config.cloned().unwrap_or_default();
    let runner = validation::ValidationRunner::new();
    runner.run(tree.root(), &db, diagnostics, &validation_config);
}
