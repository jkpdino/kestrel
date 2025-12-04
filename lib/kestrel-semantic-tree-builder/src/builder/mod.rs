//! Semantic tree building
//!
//! This module provides the `SemanticTreeBuilder` for constructing semantic trees
//! from syntax trees. The build phase creates symbol nodes and establishes the
//! parent-child hierarchy.

mod tree_builder;
mod module_validator;

pub use tree_builder::SemanticTreeBuilder;
pub use module_validator::{ModuleValidator, ModuleDeclaration};
