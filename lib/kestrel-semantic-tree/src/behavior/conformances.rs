use semantic_tree::behavior::Behavior;

use crate::{behavior::KestrelBehaviorKind, language::KestrelLanguage, ty::Ty};

/// ConformancesBehavior represents the resolved protocols that a type conforms to.
///
/// This is used for:
/// - Structs that conform to protocols (e.g., `struct Point: Drawable { }`)
/// - Protocols that inherit from other protocols (e.g., `protocol Shape: Drawable { }`)
///
/// This behavior is added during the bind phase with resolved protocol types.
/// Use the last ConformancesBehavior to get the fully resolved conformances.
#[derive(Debug, Clone)]
pub struct ConformancesBehavior {
    /// The resolved protocol types this symbol conforms to
    conformances: Vec<Ty>,
}

impl Behavior<KestrelLanguage> for ConformancesBehavior {
    fn kind(&self) -> KestrelBehaviorKind {
        KestrelBehaviorKind::Conformances
    }
}

impl ConformancesBehavior {
    /// Create a new ConformancesBehavior with the given resolved conformances
    pub fn new(conformances: Vec<Ty>) -> Self {
        ConformancesBehavior { conformances }
    }

    /// Get the resolved conformances (protocols this type conforms to)
    pub fn conformances(&self) -> &[Ty] {
        &self.conformances
    }

    /// Check if there are any conformances
    pub fn has_conformances(&self) -> bool {
        !self.conformances.is_empty()
    }
}
