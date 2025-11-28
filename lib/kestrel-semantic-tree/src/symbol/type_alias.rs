use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::behavior::Behavior;
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::{typed::TypedBehavior, visibility::VisibilityBehavior, KestrelBehaviorKind},
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
    ty::Ty,
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
    /// Create a new TypeAliasSymbol with a name, span, visibility, type, and optional parent
    pub fn new(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        ty: TypedBehavior,
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
        }
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
