use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::visibility::VisibilityBehavior,
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
    ty::Ty,
};

/// Represents a function parameter with label, binding name, and type.
///
/// Parameters support Swift-style labeled arguments:
/// - `label` is the external name used by callers (optional)
/// - `bind_name` is the internal name used in the function body
///
/// Examples:
/// - `x: Int` -> label=None, bind_name="x"
/// - `with x: Int` -> label="with", bind_name="x"
#[derive(Debug, Clone)]
pub struct Parameter {
    /// Optional external label for callers
    pub label: Option<Name>,
    /// Internal binding name used in function body
    pub bind_name: Name,
    /// The parameter's type
    pub ty: Ty,
}

impl Parameter {
    /// Create a new parameter without a label
    pub fn new(bind_name: Name, ty: Ty) -> Self {
        Self {
            label: None,
            bind_name,
            ty,
        }
    }

    /// Create a new parameter with a label
    pub fn with_label(label: Name, bind_name: Name, ty: Ty) -> Self {
        Self {
            label: Some(label),
            bind_name,
            ty,
        }
    }

    /// Get the external label (or bind_name if no label)
    pub fn external_name(&self) -> &str {
        self.label.as_ref().unwrap_or(&self.bind_name).value.as_str()
    }

    /// Get the internal binding name
    pub fn internal_name(&self) -> &str {
        &self.bind_name.value
    }
}

/// Represents a function declaration in the semantic tree.
///
/// Functions are callable entities with parameters, return types, and a body.
/// They can be:
/// - Standalone functions (at module level)
/// - Methods (within structs/classes) - indicated by having a parent
/// - Static functions (don't receive `self`)
///
/// # Callable Design
///
/// This symbol is designed to support a future callable/overloading system:
/// - Labels enable overloading by external parameter names
/// - Parameter count enables overloading by arity
/// - Return type is tracked for type checking calls
///
/// Other callable entities (initializers, closures) will share this pattern.
#[derive(Debug)]
pub struct FunctionSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
    is_static: bool,
    parameters: Vec<Parameter>,
    return_type: Ty,
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
        parameters: Vec<Parameter>,
        return_type: Ty,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        let mut builder = SymbolMetadataBuilder::new(KestrelSymbolKind::Function)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility));

        if let Some(p) = parent {
            builder = builder.with_parent(Arc::downgrade(&p));
        }

        FunctionSymbol {
            metadata: builder.build(),
            is_static,
            parameters,
            return_type,
        }
    }

    /// Check if this function is static
    pub fn is_static(&self) -> bool {
        self.is_static
    }

    /// Get the function's parameters
    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }

    /// Get the function's return type
    pub fn return_type(&self) -> &Ty {
        &self.return_type
    }

    /// Get the number of parameters (arity)
    pub fn arity(&self) -> usize {
        self.parameters.len()
    }

    /// Get the function signature as a Ty::Function
    ///
    /// This is useful for type checking and storing the function's type.
    pub fn signature_type(&self) -> Ty {
        let param_types: Vec<Ty> = self.parameters.iter().map(|p| p.ty.clone()).collect();
        Ty::function(param_types, self.return_type.clone(), self.metadata.span().clone())
    }

    /// Get parameter labels for overload resolution
    ///
    /// Returns a list of external labels (or None for unlabeled parameters).
    /// Two functions with different label patterns can coexist.
    pub fn parameter_labels(&self) -> Vec<Option<&str>> {
        self.parameters
            .iter()
            .map(|p| p.label.as_ref().map(|l| l.value.as_str()))
            .collect()
    }
}
