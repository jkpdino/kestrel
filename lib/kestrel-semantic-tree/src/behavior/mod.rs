pub mod callable;
pub mod conformances;
pub mod function_data;
pub mod typed;
pub mod valued;
pub mod visibility;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KestrelBehaviorKind {
    Callable,
    Conformances,
    FunctionData,
    ImportData,
    Typed,
    TypeAliasTyped,
    Valued,
    Visibility,
}
