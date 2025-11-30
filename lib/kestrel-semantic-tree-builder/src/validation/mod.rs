//! Validation pass infrastructure for semantic analysis
//!
//! This module provides a configurable system for running validation passes
//! on the semantic tree after binding is complete. Each pass is self-contained
//! and can be enabled/disabled individually.

mod duplicate_symbol;
mod function_body;
mod protocol_method;
mod static_context;
mod visibility_consistency;

use std::collections::HashSet;
use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::language::KestrelLanguage;
use semantic_tree::symbol::Symbol;

use crate::db::SemanticDatabase;

pub use duplicate_symbol::DuplicateSymbolPass;
pub use function_body::FunctionBodyPass;
pub use protocol_method::ProtocolMethodPass;
pub use static_context::StaticContextPass;
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
