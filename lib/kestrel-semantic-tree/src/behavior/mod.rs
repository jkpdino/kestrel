pub mod callable;
pub mod conformances;
pub mod executable;
pub mod function_data;
pub mod typed;
pub mod valued;
pub mod visibility;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KestrelBehaviorKind {
    Callable,
    Conformances,
    Executable,
    FunctionData,
    ImportData,
    Typed,
    TypeAliasTyped,
    Valued,
    Visibility,
}
