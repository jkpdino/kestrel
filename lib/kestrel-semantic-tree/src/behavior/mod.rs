pub mod typed;
pub mod visibility;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KestrelBehaviorKind {
    ImportData,
    Typed,
    TypeAliasTyped,
    Visibility,
}
