use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::behavior::Behavior;
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::{typed::TypedBehavior, visibility::VisibilityBehavior, KestrelBehaviorKind},
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
    symbol::type_parameter::TypeParameterSymbol,
    ty::{Ty, WhereClause},
};

#[derive(Debug)]
pub struct TypeAliasSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
    /// Type parameters for generic type aliases, e.g., `type Pair[T] = (T, T)`
    type_parameters: Vec<Arc<TypeParameterSymbol>>,
    /// Where clause constraints for type parameters
    where_clause: WhereClause,
}

impl Symbol<KestrelLanguage> for TypeAliasSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl TypeAliasSymbol {
    /// Create a new TypeAliasSymbol with a name, span, visibility, type, and optional parent
    pub fn new(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        ty: TypedBehavior,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        Self::with_generics(name, span, visibility, ty, Vec::new(), WhereClause::new(), parent)
    }

    /// Create a new generic TypeAliasSymbol with type parameters and where clause
    pub fn with_generics(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        ty: TypedBehavior,
        type_parameters: Vec<Arc<TypeParameterSymbol>>,
        where_clause: WhereClause,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        let mut builder = SymbolMetadataBuilder::new(KestrelSymbolKind::TypeAlias)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility))
            .with_behavior(Arc::new(ty));

        if let Some(p) = parent {
            builder = builder.with_parent(Arc::downgrade(&p));
        }

        TypeAliasSymbol {
            metadata: builder.build(),
            type_parameters,
            where_clause,
        }
    }

    /// Get the type parameters for this type alias
    pub fn type_parameters(&self) -> &[Arc<TypeParameterSymbol>] {
        &self.type_parameters
    }

    /// Check if this type alias is generic (has type parameters)
    pub fn is_generic(&self) -> bool {
        !self.type_parameters.is_empty()
    }

    /// Get the number of type parameters
    pub fn type_parameter_count(&self) -> usize {
        self.type_parameters.len()
    }

    /// Get the where clause for this type alias
    pub fn where_clause(&self) -> &WhereClause {
        &self.where_clause
    }
}

/// TypeAliasTypedBehavior represents the resolved type information for a type alias
///
/// This behavior is added during the binding phase after resolving all path types
/// in the aliased type. The original TypedBehavior contains the syntactic type
/// (which may have unresolved Path variants), while this behavior contains the
/// fully resolved type.
#[derive(Debug, Clone)]
pub struct TypeAliasTypedBehavior {
    /// The fully resolved type that this type alias refers to
    resolved_ty: Ty,
}

impl Behavior<KestrelLanguage> for TypeAliasTypedBehavior {
    fn kind(&self) -> KestrelBehaviorKind {
        KestrelBehaviorKind::TypeAliasTyped
    }
}

impl TypeAliasTypedBehavior {
    /// Create a new TypeAliasTypedBehavior with the resolved type
    pub fn new(resolved_ty: Ty) -> Self {
        TypeAliasTypedBehavior { resolved_ty }
    }

    /// Get the resolved type
    pub fn resolved_ty(&self) -> &Ty {
        &self.resolved_ty
    }
}
