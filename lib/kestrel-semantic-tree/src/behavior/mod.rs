pub mod callable;
pub mod typed;
pub mod visibility;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KestrelBehaviorKind {
    Callable,
    ImportData,
    Typed,
    TypeAliasTyped,
    Visibility,
}
