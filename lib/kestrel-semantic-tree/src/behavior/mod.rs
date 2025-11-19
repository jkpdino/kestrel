pub mod typed;
pub mod visibility;

use semantic_tree::behavior::BehaviorKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KestrelBehaviorKind {
    ImportData,
    Typed,
    TypeAliasTyped,
    Visibility,
}

impl BehaviorKind for KestrelBehaviorKind {}
