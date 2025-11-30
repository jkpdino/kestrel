use std::sync::Arc;

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
pub struct StructSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
    /// Type parameters for generic structs, e.g., `struct Box[T]`
    type_parameters: Vec<Arc<TypeParameterSymbol>>,
    /// Where clause constraints for type parameters
    where_clause: WhereClause,
    /// Protocols this struct conforms to, e.g., `struct Point: Drawable { }`
    conformances: Vec<Ty>,
}

impl Symbol<KestrelLanguage> for StructSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl StructSymbol {
    /// Create a new StructSymbol with a name, span, visibility, and optional parent
    pub fn new(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        Self::with_generics(name, span, visibility, Vec::new(), WhereClause::new(), Vec::new(), parent)
    }

    /// Create a new generic StructSymbol with type parameters, where clause, and conformances
    pub fn with_generics(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        type_parameters: Vec<Arc<TypeParameterSymbol>>,
        where_clause: WhereClause,
        conformances: Vec<Ty>,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        let mut builder = SymbolMetadataBuilder::new(KestrelSymbolKind::Struct)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility));

        if let Some(p) = parent {
            builder = builder.with_parent(Arc::downgrade(&p));
        }

        StructSymbol {
            metadata: builder.build(),
            type_parameters,
            where_clause,
            conformances,
        }
    }

    /// Get the type parameters for this struct
    pub fn type_parameters(&self) -> &[Arc<TypeParameterSymbol>] {
        &self.type_parameters
    }

    /// Check if this struct is generic (has type parameters)
    pub fn is_generic(&self) -> bool {
        !self.type_parameters.is_empty()
    }

    /// Get the number of type parameters
    pub fn type_parameter_count(&self) -> usize {
        self.type_parameters.len()
    }

    /// Get the where clause for this struct
    pub fn where_clause(&self) -> &WhereClause {
        &self.where_clause
    }

    /// Get the protocols this struct conforms to
    pub fn conformances(&self) -> &[Ty] {
        &self.conformances
    }

    /// Check if this struct has any conformances
    pub fn has_conformances(&self) -> bool {
        !self.conformances.is_empty()
    }
}
