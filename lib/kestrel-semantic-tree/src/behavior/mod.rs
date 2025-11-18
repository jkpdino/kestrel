pub mod visibility;
pub mod typed;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KestrelBehaviorKind {
    Visibility,
    Typed,
    TypeAliasTyped,
}
