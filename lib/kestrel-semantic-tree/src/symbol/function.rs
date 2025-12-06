use std::sync::{Arc, RwLock};

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::{
    behavior::callable::{CallableBehavior, CallableSignature},
    behavior::function_data::FunctionDataBehavior,
    behavior::visibility::VisibilityBehavior,
    behavior_ext::BehaviorExt,
    language::KestrelLanguage,
    symbol::kind::KestrelSymbolKind,
    symbol::local::{Local, LocalId},
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
///
/// # Type Resolution
///
/// During build phase, basic symbol information is captured but `CallableBehavior` is not yet added.
/// During bind phase, `CallableBehavior` is added with resolved types.
/// Query methods like `return_type()` and `signature()` return `None`/defaults until bind occurs.
#[derive(Debug)]
pub struct FunctionSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
    is_static: bool,
    has_body: bool,
    /// Type parameters for generic functions, e.g., `func identity[T](value: T) -> T`
    type_parameters: Vec<Arc<TypeParameterSymbol>>,
    /// Where clause constraints for type parameters.
    /// During BUILD phase, bounds are Ty::error() placeholders.
    /// During BIND phase, bounds are updated to resolved protocol types.
    where_clause: RwLock<WhereClause>,
    /// Local variables within this function (populated during body resolution)
    /// This includes function parameters and any let/var declarations.
    /// Variables with the same name due to shadowing have different LocalIds.
    locals: RwLock<Vec<Local>>,
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
    ///
    /// NOTE: CallableBehavior is NOT added here. It will be added during the bind phase
    /// when types are resolved. This avoids having two CallableBehaviors (one with
    /// unresolved placeholder types, one with resolved types).
    pub fn with_generics(
        name: Name,
        span: Span,
        visibility: VisibilityBehavior,
        is_static: bool,
        has_body: bool,
        _parameters: Vec<Parameter>,
        _return_type: Ty,
        type_parameters: Vec<Arc<TypeParameterSymbol>>,
        where_clause: WhereClause,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        // Create the function data behavior
        let function_data = FunctionDataBehavior::new(has_body, is_static);

        // Note: CallableBehavior is added during bind phase with resolved types
        let mut builder = SymbolMetadataBuilder::new(KestrelSymbolKind::Function)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span)
            .with_behavior(Arc::new(visibility))
            .with_behavior(Arc::new(function_data));

        if let Some(p) = parent {
            builder = builder.with_parent(Arc::downgrade(&p));
        }

        FunctionSymbol {
            metadata: builder.build(),
            is_static,
            has_body,
            type_parameters,
            where_clause: RwLock::new(where_clause),
            locals: RwLock::new(Vec::new()),
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

    /// Get the callable behavior from metadata.
    ///
    /// Returns `None` if bind phase hasn't occurred yet (CallableBehavior is added during bind).
    fn get_callable(&self) -> Option<CallableBehavior> {
        self.metadata.callable_behavior()
    }

    /// Get the callable behavior (cloned)
    pub fn callable(&self) -> Option<CallableBehavior> {
        self.get_callable()
    }

    /// Get the function's return type
    ///
    /// Returns the resolved return type if bind has occurred.
    pub fn return_type(&self) -> Ty {
        self.get_callable()
            .map(|c| c.return_type().clone())
            .unwrap_or_else(|| Ty::error(0..0))
    }

    /// Get the function's parameters
    pub fn parameters(&self) -> Vec<Parameter> {
        self.get_callable()
            .map(|c| c.parameters().to_vec())
            .unwrap_or_default()
    }

    /// Get the number of parameters (arity)
    pub fn arity(&self) -> usize {
        self.get_callable().map(|c| c.arity()).unwrap_or(0)
    }

    /// Get the callable signature for overload resolution and duplicate detection.
    ///
    /// Two functions with the same signature are considered duplicates.
    pub fn signature(&self) -> CallableSignature {
        self.get_callable()
            .map(|c| c.signature(&self.metadata.name().value))
            .unwrap_or_else(|| CallableSignature::new(self.metadata.name().value.clone(), vec![], vec![]))
    }

    /// Get the function signature as a Ty::Function
    ///
    /// This is useful for type checking and storing the function's type.
    pub fn function_type(&self) -> Ty {
        self.get_callable()
            .map(|c| c.function_type())
            .unwrap_or_else(|| Ty::error(0..0))
    }

    /// Get parameter labels for display/debugging
    ///
    /// Returns a list of external labels (or None for unlabeled parameters).
    pub fn parameter_labels(&self) -> Vec<Option<String>> {
        self.get_callable()
            .map(|c| c.parameters().iter().map(|p| p.external_label().map(|s| s.to_string())).collect())
            .unwrap_or_default()
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

    /// Get the where clause for this function (cloned)
    pub fn where_clause(&self) -> WhereClause {
        self.where_clause.read().unwrap().clone()
    }

    /// Update a specific bound in the where clause by constraint index.
    ///
    /// This is called during the BIND phase to replace placeholder Ty::error()
    /// bounds with resolved protocol types.
    ///
    /// # Arguments
    /// * `constraint_index` - The index of the constraint in where_clause.constraints
    /// * `bound_index` - The index of the bound within that constraint
    /// * `resolved_bound` - The resolved protocol type
    pub fn update_where_clause_bound(
        &self,
        constraint_index: usize,
        bound_index: usize,
        resolved_bound: Ty,
    ) {
        let mut wc = self.where_clause.write().unwrap();
        if constraint_index < wc.constraints.len() {
            if let crate::ty::Constraint::TypeBound { bounds, .. } = &mut wc.constraints[constraint_index] {
                if bound_index < bounds.len() {
                    bounds[bound_index] = resolved_bound;
                }
            }
        }
    }

    /// Add a new local variable to this function.
    /// Returns the LocalId assigned to the new local.
    pub fn add_local(&self, name: String, ty: Ty, mutable: bool, span: Span) -> LocalId {
        let mut locals = self.locals.write().unwrap();
        let id = LocalId::new(locals.len());
        locals.push(Local::new(id, name, ty, mutable, span));
        id
    }

    /// Get a local by its ID
    pub fn get_local(&self, id: LocalId) -> Option<Local> {
        let locals = self.locals.read().unwrap();
        locals.get(id.index()).cloned()
    }

    /// Get all locals in this function
    pub fn locals(&self) -> Vec<Local> {
        self.locals.read().unwrap().clone()
    }

    /// Get the number of locals
    pub fn local_count(&self) -> usize {
        self.locals.read().unwrap().len()
    }
}
