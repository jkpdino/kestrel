//! Resolution and binding
//!
//! This module provides components for the bind phase of semantic analysis:
//! - `SemanticBinder`: Orchestrates binding of all symbols
//! - `TypeResolver`: Resolves types from syntax nodes
//! - `VisibilityChecker`: Checks symbol visibility from a context
//! - `LocalScope`: Manages local variable scopes in function bodies

mod binder;
pub mod type_resolver;
pub mod visibility;
mod local_scope;

pub use binder::SemanticBinder;
pub use type_resolver::TypeResolver;
pub use visibility::VisibilityChecker;
pub use local_scope::LocalScope;
