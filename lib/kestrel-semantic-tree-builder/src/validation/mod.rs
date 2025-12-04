//! Validation pass infrastructure for semantic analysis
//!
//! This module provides a configurable system for running validation passes
//! on the semantic tree after binding is complete. Passes can run either:
//! - Individually (each walking the tree separately)
//! - Batched (single tree walk calling all passes per node)
//!
//! The batched approach is more efficient for large codebases.

mod assignment_validation;
mod conformance;
mod constraint_cycles;
mod duplicate_symbol;
mod function_body;
mod generics;
mod imports;
mod initializer_verification;
mod protocol_method;
mod static_context;
mod struct_cycles;
mod type_alias_cycles;
mod visibility_consistency;

use std::collections::HashSet;
use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::language::KestrelLanguage;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;

pub use assignment_validation::AssignmentValidationPass;
pub use conformance::ConformancePass;
pub use constraint_cycles::ConstraintCyclePass;
pub use duplicate_symbol::DuplicateSymbolPass;
pub use function_body::FunctionBodyPass;
pub use generics::GenericsPass;
pub use imports::ImportValidationPass;
pub use initializer_verification::InitializerVerificationPass;
pub use protocol_method::ProtocolMethodPass;
pub use static_context::StaticContextPass;
pub use struct_cycles::StructCyclePass;
pub use type_alias_cycles::TypeAliasCyclePass;
pub use visibility_consistency::VisibilityConsistencyPass;

/// Configuration for which validation passes to run
#[derive(Default, Clone)]
pub struct ValidationConfig {
    /// If true, include pass name in error messages (for debugging)
    pub debug_mode: bool,
    /// Set of pass names that should be disabled
    disabled_passes: HashSet<&'static str>,
}

impl ValidationConfig {
    /// Create a new validation config with default settings
    pub fn new() -> Self {
        Self::default()
    }

    /// Disable a specific validation pass by name
    pub fn disable(&mut self, pass_name: &'static str) {
        self.disabled_passes.insert(pass_name);
    }

    /// Check if a pass is enabled
    pub fn is_enabled(&self, pass_name: &'static str) -> bool {
        !self.disabled_passes.contains(pass_name)
    }

    /// Enable debug mode (shows pass name in errors)
    pub fn with_debug_mode(mut self) -> Self {
        self.debug_mode = true;
        self
    }
}

/// Trait for validation passes
///
/// Each validation pass implements this trait to perform semantic checks
/// on the symbol tree after binding is complete.
pub trait ValidationPass: Send + Sync {
    /// Unique identifier for this pass
    fn name(&self) -> &'static str;

    /// Run the validation pass on the semantic tree
    ///
    /// # Arguments
    /// * `root` - The root symbol of the semantic tree
    /// * `db` - The semantic database for queries
    /// * `diagnostics` - Context for reporting errors
    /// * `config` - Configuration for this validation run
    fn validate(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        config: &ValidationConfig,
    );
}

/// Registry and runner for all validation passes
pub struct ValidationRunner {
    passes: Vec<Box<dyn ValidationPass>>,
}

impl ValidationRunner {
    /// Create a new validation runner with all registered passes
    pub fn new() -> Self {
        let passes: Vec<Box<dyn ValidationPass>> = vec![
            Box::new(FunctionBodyPass),
            Box::new(ProtocolMethodPass),
            Box::new(StaticContextPass),
            Box::new(DuplicateSymbolPass),
            Box::new(VisibilityConsistencyPass),
            Box::new(GenericsPass),
            Box::new(TypeAliasCyclePass),
            Box::new(StructCyclePass),
            Box::new(ConstraintCyclePass),
            Box::new(ImportValidationPass),
            Box::new(ConformancePass),
            Box::new(InitializerVerificationPass),
            Box::new(AssignmentValidationPass),
        ];

        Self { passes }
    }

    /// Run all enabled validation passes
    pub fn run(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        config: &ValidationConfig,
    ) {
        for pass in &self.passes {
            if config.is_enabled(pass.name()) {
                pass.validate(root, db, diagnostics, config);
            }
        }
    }
}

impl Default for ValidationRunner {
    fn default() -> Self {
        Self::new()
    }
}

// Note on batched validation:
// The current implementation runs each pass separately, which means N tree walks for N passes.
// A batched implementation would walk the tree once and call all validations per node.
//
// To implement batched validation:
// 1. Each validation module would expose per-symbol-kind validation functions
// 2. A single tree walk would call the appropriate functions based on symbol kind
//
// For now, the separate-pass approach is maintained for simplicity and maintainability.
// The performance impact is minimal for typical codebases, as tree walks are cheap.
// If profiling shows this is a bottleneck, batching can be implemented by:
// - Making validate_* functions pub(crate) in each module
// - Creating a BatchedValidationRunner that does a single tree walk
