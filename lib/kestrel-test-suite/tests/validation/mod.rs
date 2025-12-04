//! Tests for semantic validation
//!
//! This module contains tests for:
//! - Mutability checking (let vs var)
//! - Circular reference detection
//! - Visibility consistency
//! - Duplicate symbol detection
//! - Protocol conformance validation

mod mutability;
mod cycles;
mod misc;
