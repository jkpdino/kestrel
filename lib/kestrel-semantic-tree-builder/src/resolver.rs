use std::collections::HashMap;
use std::sync::Arc;

use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::symbol::Symbol;

use crate::resolvers::{ClassResolver, ImportResolver, ModuleResolver, TerminalResolver};

/// Trait for resolving syntax nodes into semantic symbols
pub trait Resolver {
    /// Build phase: create symbol from syntax node and add to parent
    /// Returns the created symbol for tree walker recursion
    fn build_declaration(
        &self,
        syntax: &SyntaxNode,
        source: &str,
        parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>>;

    /// Binding phase: resolve references and establish relationships
    fn bind_declaration(
        &self,
        _symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        _context: &BindingContext,
    ) {
        // Default: do nothing
    }

    /// Whether this node is terminal (stops tree traversal)
    fn is_terminal(&self) -> bool {
        false
    }
}

/// Context for the binding phase
pub struct BindingContext {
    // Empty for now - will be populated when binding phase is implemented
}

/// Registry mapping SyntaxKind to Resolver implementations
pub struct ResolverRegistry {
    resolvers: HashMap<SyntaxKind, Box<dyn Resolver>>,
}

impl ResolverRegistry {
    /// Create a new registry with all resolvers registered
    pub fn new() -> Self {
        let mut resolvers: HashMap<SyntaxKind, Box<dyn Resolver>> = HashMap::new();

        // Register declaration resolvers
        resolvers.insert(
            SyntaxKind::ClassDeclaration,
            Box::new(ClassResolver),
        );
        resolvers.insert(
            SyntaxKind::ModuleDeclaration,
            Box::new(ModuleResolver),
        );
        resolvers.insert(
            SyntaxKind::ImportDeclaration,
            Box::new(ImportResolver),
        );

        // Register terminal resolvers (separate instances for each)
        resolvers.insert(
            SyntaxKind::Visibility,
            Box::new(TerminalResolver),
        );
        resolvers.insert(
            SyntaxKind::Name,
            Box::new(TerminalResolver),
        );

        // Note: ClassBody has no resolver - it gets walked automatically

        ResolverRegistry { resolvers }
    }

    /// Get a resolver for a given SyntaxKind
    pub fn get(&self, kind: SyntaxKind) -> Option<&dyn Resolver> {
        self.resolvers.get(&kind).map(|b| b.as_ref())
    }
}
