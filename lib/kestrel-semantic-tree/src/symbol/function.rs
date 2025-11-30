use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::callable::{CallableBehavior, CallableSignature},
    behavior::function_data::FunctionDataBehavior,
    behavior::visibility::VisibilityBehavior,
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
    symbol::type_parameter::TypeParameterSymbol,
    ty::{Ty, WhereClause},
};

// Re-export CallableParameter as Parameter for backwards compatibility
pub use crate::behavior::callable::CallableParameter as Parameter;

/// Represents a function declaration in the semantic tree.
///
/// Functions are callable entities with parameters, return types, and a body.
/// They can be:
/// - Standalone functions (at module level)
/// - Methods (within structs/classes) - indicated by having a parent
/// - Static functions (don't receive `self`)
///
/// # Callable/Overloading System
///
/// Functions use `CallableBehavior` for overload resolution:
/// - Labels enable overloading by external parameter names
/// - Parameter types enable type-based overloading
/// - Parameter count enables overloading by arity
///
/// Two functions with the same `CallableSignature` are duplicates (error).
/// Functions with different signatures can coexist as overloads.
#[derive(Debug)]
pub struct FunctionSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
    is_static: bool,
    has_body: bool,
    callable: CallableBehavior,
    /// Type parameters for generic functions, e.g., `func identity[T](value: T) -> T`
    type_parameters: Vec<Arc<TypeParameterSymbol>>,
    /// Where clause constraints for type parameters
    where_clause: WhereClause,
}

impl Symbol<KestrelLanguage> for FunctionSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl FunctionSymbol {
    /// Create a new FunctionSymbol
    pub fn new(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        is_static: bool,
        has_body: bool,
        parameters: Vec<Parameter>,
        return_type: Ty,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        Self::with_generics(
            name,
            span,
            visibility,
            is_static,
            has_body,
            parameters,
            return_type,
            Vec::new(),
            WhereClause::new(),
            parent,
        )
    }

    /// Create a new generic FunctionSymbol with type parameters and where clause
    pub fn with_generics(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        is_static: bool,
        has_body: bool,
        parameters: Vec<Parameter>,
        return_type: Ty,
        type_parameters: Vec<Arc<TypeParameterSymbol>>,
        where_clause: WhereClause,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        // Create the callable behavior
        let callable = CallableBehavior::new(parameters, return_type, span.clone());

        // Create the function data behavior
        let function_data = FunctionDataBehavior::new(has_body, is_static);

        let mut builder = SymbolMetadataBuilder::new(KestrelSymbolKind::Function)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility))
            .with_behavior(Arc::new(callable.clone()))
            .with_behavior(Arc::new(function_data));

        if let Some(p) = parent {
            builder = builder.with_parent(Arc::downgrade(&p));
        }

        FunctionSymbol {
            metadata: builder.build(),
            is_static,
            has_body,
            callable,
            type_parameters,
            where_clause,
        }
    }

    /// Check if this function is static
    pub fn is_static(&self) -> bool {
        self.is_static
    }

    /// Check if this function has a body
    pub fn has_body(&self) -> bool {
        self.has_body
    }

    /// Get the callable behavior
    pub fn callable(&self) -> &CallableBehavior {
        &self.callable
    }

    /// Get the function's parameters
    pub fn parameters(&self) -> &[Parameter] {
        self.callable.parameters()
    }

    /// Get the function's return type
    pub fn return_type(&self) -> &Ty {
        self.callable.return_type()
    }

    /// Get the number of parameters (arity)
    pub fn arity(&self) -> usize {
        self.callable.arity()
    }

    /// Get the callable signature for overload resolution and duplicate detection.
    ///
    /// Two functions with the same signature are considered duplicates.
    pub fn signature(&self) -> CallableSignature {
        self.callable.signature(&self.metadata.name().value)
    }

    /// Get the function signature as a Ty::Function
    ///
    /// This is useful for type checking and storing the function's type.
    pub fn function_type(&self) -> Ty {
        self.callable.function_type()
    }

    /// Get parameter labels for display/debugging
    ///
    /// Returns a list of external labels (or None for unlabeled parameters).
    pub fn parameter_labels(&self) -> Vec<Option<&str>> {
        self.callable.parameter_labels()
    }

    /// Get the type parameters for this function
    pub fn type_parameters(&self) -> &[Arc<TypeParameterSymbol>] {
        &self.type_parameters
    }

    /// Check if this function is generic (has type parameters)
    pub fn is_generic(&self) -> bool {
        !self.type_parameters.is_empty()
    }

    /// Get the number of type parameters
    pub fn type_parameter_count(&self) -> usize {
        self.type_parameters.len()
    }

    /// Get the where clause for this function
    pub fn where_clause(&self) -> &WhereClause {
        &self.where_clause
    }
}
