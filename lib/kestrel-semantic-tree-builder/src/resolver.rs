use std::collections::HashMap;
use std::sync::Arc;

use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};
use semantic_tree::cycle::CycleDetector;
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::resolvers::{ClassResolver, FieldResolver, FunctionResolver, ImportResolver, ModuleResolver, StructResolver, TerminalResolver, TypeAliasResolver};

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
}

impl BindingContext<'_> {
    /// Get the file_id for a symbol by walking up to its SourceFile parent.
    ///
    /// This enables cross-file diagnostics - errors can reference declarations
    /// in other files by using the correct file_id for each span.
    pub fn file_id_for_symbol(&self, symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<usize> {
        let mut current = Some(symbol.clone());
        while let Some(s) = current {
            if s.metadata().kind() == KestrelSymbolKind::SourceFile {
                let file_name = s.metadata().name().value.clone();
                return self.diagnostics.get_file_id(&file_name);
            }
            current = s.metadata().parent();
        }
        None
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
        resolvers.insert(
            SyntaxKind::TypeAliasDeclaration,
            Box::new(TypeAliasResolver),
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
