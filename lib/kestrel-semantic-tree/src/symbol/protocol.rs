use std::sync::{Arc, RwLock};

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::visibility::VisibilityBehavior,
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
    symbol::type_parameter::TypeParameterSymbol,
    ty::{Ty, WhereClause},
};

#[derive(Debug)]
pub struct ProtocolSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
    /// Type parameters for generic protocols, e.g., `protocol Iterator[T]`
    type_parameters: Vec<Arc<TypeParameterSymbol>>,
    /// Where clause constraints for type parameters
    where_clause: WhereClause,
    /// Inherited protocols, e.g., `protocol Shape: Drawable { }`
    /// Initially contains unresolved placeholder types, resolved during bind phase.
    inherited_protocols: RwLock<Vec<Ty>>,
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
        Self::with_generics(name, span, visibility, Vec::new(), WhereClause::new(), Vec::new(), parent)
    }

    /// Create a new generic ProtocolSymbol with type parameters, where clause, and inherited protocols
    pub fn with_generics(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        type_parameters: Vec<Arc<TypeParameterSymbol>>,
        where_clause: WhereClause,
        inherited_protocols: Vec<Ty>,
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
            inherited_protocols: RwLock::new(inherited_protocols),
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

    /// Get the protocols this protocol inherits from
    pub fn inherited_protocols(&self) -> Vec<Ty> {
        self.inherited_protocols.read().unwrap().clone()
    }

    /// Check if this protocol inherits from any other protocols
    pub fn has_inherited_protocols(&self) -> bool {
        !self.inherited_protocols.read().unwrap().is_empty()
    }

    /// Set the resolved inherited protocols (called during bind phase)
    pub fn set_inherited_protocols(&self, protocols: Vec<Ty>) {
        *self.inherited_protocols.write().unwrap() = protocols;
    }
}
