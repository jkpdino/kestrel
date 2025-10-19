use std::sync::Arc;

use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_syntax_tree::SyntaxNode;
use semantic_tree::symbol::Symbol;

use crate::resolver::Resolver;

/// Resolver for module declarations
/// TODO: Implement module symbols in kestrel-semantic-tree
pub struct ModuleResolver;

impl Resolver for ModuleResolver {
    fn build_declaration(
        &self,
        _syntax: &SyntaxNode,
        _source: &str,
        _parent: Option<&Arc<dyn Symbol<KestrelLanguage>>>,
        _root: &Arc<dyn Symbol<KestrelLanguage>>,
    ) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
        // TODO: Implement ModuleSymbol
        None
    }
}
