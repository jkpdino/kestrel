use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::behavior::Behavior;
use semantic_tree::symbol::{Symbol, SymbolId, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::KestrelBehaviorKind,
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
};

#[derive(Debug)]
pub struct ImportSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
}

impl Symbol<KestrelLanguage> for ImportSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl ImportSymbol {
    /// Create a new ImportSymbol
    pub fn new(
        name: Name,
        parent: Arc<dyn Symbol<KestrelLanguage>>,
        span: Span,
    ) -> Self {
        let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::Import)
            .with_parent(Arc::downgrade(&parent))
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .build();

        ImportSymbol { metadata }
    }
}

/// Import data behavior stores the parsed import information
#[derive(Debug, Clone)]
pub struct ImportDataBehavior {
    /// The module path (e.g., ["A", "B", "C"] for "import A.B.C")
    pub module_path: Vec<String>,
    /// Optional alias for the module (e.g., "D" for "import A.B.C as D")
    pub alias: Option<String>,
    /// Specific items to import (e.g., [("Foo", None), ("Bar", Some("Baz"))])
    /// Empty if importing the entire module
    pub items: Vec<ImportItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportItem {
    /// The name of the symbol to import
    pub name: String,
    /// Optional alias for this specific import
    pub alias: Option<String>,
    /// Resolved symbol ID (filled during bind phase)
    pub target_id: Option<SymbolId>,
}

impl Behavior<KestrelLanguage> for ImportDataBehavior {
    fn kind(&self) -> KestrelBehaviorKind {
        KestrelBehaviorKind::ImportData
    }
}

impl ImportDataBehavior {
    pub fn new(
        module_path: Vec<String>,
        alias: Option<String>,
        items: Vec<ImportItem>,
    ) -> Self {
        ImportDataBehavior {
            module_path,
            alias,
            items,
        }
    }

    pub fn module_path(&self) -> &[String] {
        &self.module_path
    }

    pub fn alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }

    pub fn items(&self) -> &[ImportItem] {
        &self.items
    }
}
