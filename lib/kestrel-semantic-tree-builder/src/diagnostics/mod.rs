//! Diagnostic error types for the semantic tree builder.
//!
//! This module provides structured error types that implement `IntoDiagnostic`,
//! organized by category:
//!
//! - `module` - Module declaration errors
//! - `type_resolution` - Type lookup and generic instantiation errors
//! - `protocol` - Protocol conformance and inheritance errors
//! - `visibility` - Visibility consistency errors
//! - `declaration` - Duplicate symbols and missing body errors

mod module;
mod type_resolution;
mod protocol;
mod visibility;
mod declaration;

pub use module::*;
pub use type_resolution::*;
pub use protocol::*;
pub use visibility::*;
pub use declaration::*;
