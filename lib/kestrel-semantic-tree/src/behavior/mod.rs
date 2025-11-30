pub mod callable;
pub mod function_data;
pub mod typed;
pub mod visibility;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KestrelBehaviorKind {
    Callable,
    FunctionData,
    ImportData,
    Typed,
    TypeAliasTyped,
    Visibility,
}
