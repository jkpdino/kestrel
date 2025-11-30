use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::visibility::VisibilityBehavior,
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
    symbol::type_parameter::TypeParameterSymbol,
    ty::WhereClause,
};

#[derive(Debug)]
pub struct ProtocolSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
    /// Type parameters for generic protocols, e.g., `protocol Iterator[T]`
    type_parameters: Vec<Arc<TypeParameterSymbol>>,
    /// Where clause constraints for type parameters
    where_clause: WhereClause,
}

impl Symbol<KestrelLanguage> for ProtocolSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl ProtocolSymbol {
    /// Create a new ProtocolSymbol with a name, span, visibility, and optional parent
    pub fn new(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        Self::with_generics(name, span, visibility, Vec::new(), WhereClause::new(), parent)
    }

    /// Create a new generic ProtocolSymbol with type parameters and where clause
    pub fn with_generics(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        type_parameters: Vec<Arc<TypeParameterSymbol>>,
        where_clause: WhereClause,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        let mut builder = SymbolMetadataBuilder::new(KestrelSymbolKind::Protocol)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility));

        if let Some(p) = parent {
            builder = builder.with_parent(Arc::downgrade(&p));
        }

        ProtocolSymbol {
            metadata: builder.build(),
            type_parameters,
            where_clause,
        }
    }

    /// Get the type parameters for this protocol
    pub fn type_parameters(&self) -> &[Arc<TypeParameterSymbol>] {
        &self.type_parameters
    }

    /// Check if this protocol is generic (has type parameters)
    pub fn is_generic(&self) -> bool {
        !self.type_parameters.is_empty()
    }

    /// Get the number of type parameters
    pub fn type_parameter_count(&self) -> usize {
        self.type_parameters.len()
    }

    /// Get the where clause for this protocol
    pub fn where_clause(&self) -> &WhereClause {
        &self.where_clause
    }
}
