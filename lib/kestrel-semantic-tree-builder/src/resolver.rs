use std::collections::HashMap;
use std::sync::Arc;

use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::cycle::CycleDetector;
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::resolvers::{FieldResolver, FunctionResolver, ImportResolver, InitializerResolver, ModuleResolver, ProtocolResolver, StructResolver, TerminalResolver, TypeAliasResolver};

/// Storage for source code by file, keyed by file name
pub type SourceMap = HashMap<String, String>;

/// Storage for syntax nodes by symbol ID, allowing bind phase to access syntax
pub type SyntaxMap = HashMap<SymbolId, SyntaxNode>;

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
    ///
    /// The syntax node is the same node that was passed to build_declaration,
    /// allowing resolvers to extract type information directly from syntax
    /// during binding rather than storing intermediate Path representations.
    fn bind_declaration(
        &self,
        _symbol: &Arc<dyn Symbol<KestrelLanguage>>,
        _syntax: &SyntaxNode,
        _context: &mut BindingContext,
    ) {
        // Default: do nothing
    }

    /// Whether this node is terminal (stops tree traversal)
    fn is_terminal(&self) -> bool {
        false
    }
}

/// Context for the binding phase
pub struct BindingContext<'a> {
    /// Salsa database for queries
    pub db: &'a dyn crate::queries::Db,
    /// Diagnostics collector
    pub diagnostics: &'a mut kestrel_reporting::DiagnosticContext,
    /// Current file ID for error reporting
    pub file_id: usize,
    /// Cycle detector for type alias resolution
    pub type_alias_cycle_detector: &'a mut CycleDetector<SymbolId>,
    /// Source code by file name
    pub sources: &'a SourceMap,
}

impl BindingContext<'_> {
    /// Get file_id and source code for a symbol in one call.
    ///
    /// This is the preferred method for resolvers that need both file_id (for diagnostics)
    /// and source code (for span calculation). It performs a single parent-chain traversal.
    ///
    /// Returns (file_id, source) where file_id falls back to self.file_id and source is cloned.
    pub fn get_file_context(&self, symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> (usize, String) {
        match crate::utils::get_source_file_info(symbol, self.diagnostics) {
            Some(info) => {
                let source = self.sources.get(&info.file_name).cloned().unwrap_or_default();
                (info.file_id, source)
            }
            None => (self.file_id, String::new()),
        }
    }
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
            SyntaxKind::ModuleDeclaration,
            Box::new(ModuleResolver),
        );
        resolvers.insert(
            SyntaxKind::ImportDeclaration,
            Box::new(ImportResolver),
        );
        resolvers.insert(
            SyntaxKind::TypeAliasDeclaration,
            Box::new(TypeAliasResolver),
        );
        resolvers.insert(
            SyntaxKind::ProtocolDeclaration,
            Box::new(ProtocolResolver),
        );
        resolvers.insert(
            SyntaxKind::StructDeclaration,
            Box::new(StructResolver),
        );
        resolvers.insert(
            SyntaxKind::FieldDeclaration,
            Box::new(FieldResolver),
        );
        resolvers.insert(
            SyntaxKind::FunctionDeclaration,
            Box::new(FunctionResolver),
        );
        resolvers.insert(
            SyntaxKind::InitializerDeclaration,
            Box::new(InitializerResolver),
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

        ResolverRegistry { resolvers }
    }

    /// Get a resolver for a given SyntaxKind
    pub fn get(&self, kind: SyntaxKind) -> Option<&dyn Resolver> {
        self.resolvers.get(&kind).map(|b| b.as_ref())
    }
}
