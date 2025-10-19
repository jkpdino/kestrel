use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::visibility::VisibilityBehavior,
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind
};

#[derive(Debug)]
pub struct FnSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
}

impl Symbol<KestrelLanguage> for FnSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl FnSymbol {
    /// Create a new FnSymbol with a name, span, and visibility
    pub fn new(name: Name, span: Span, visibility: VisibilityBehavior) -> Self {
        let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::Fn)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility))
            .build();

        FnSymbol { metadata }
    }
}
