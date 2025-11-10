use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::{typed::TypedBehavior, visibility::VisibilityBehavior},
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
};

#[derive(Debug)]
pub struct TypeAliasSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
}

impl Symbol<KestrelLanguage> for TypeAliasSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        return &self.metadata;
    }
}

impl TypeAliasSymbol {
    /// Create a new TypeAliasSymbol with a name, span, visibility, and type
    pub fn new(name: Name, span: Span, visibility: VisibilityBehavior, ty: TypedBehavior) -> Self {
        let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::TypeAlias)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility))
            .with_behavior(Arc::new(ty))
            .build();

        TypeAliasSymbol { metadata }
    }
}
