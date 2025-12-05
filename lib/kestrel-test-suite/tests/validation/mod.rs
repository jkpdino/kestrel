//! Tests for semantic validation
//!
//! This module contains tests for:
//! - Mutability checking (let vs var)
//! - Circular reference detection
//! - Visibility consistency
//! - Duplicate symbol detection
//! - Protocol conformance validation
//! - Initializer verification (field initialization, control flow)
//! - Dead code detection
//! - Type checking

mod mutability;
mod cycles;
mod misc;
mod initializers;
mod dead_code;
mod exhaustive_return;
mod type_checking;
