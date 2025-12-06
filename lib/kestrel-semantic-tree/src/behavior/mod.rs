pub mod callable;
pub mod conformances;
pub mod executable;
pub mod function_data;
pub mod generics;
pub mod member_access;
pub mod typed;
pub mod valued;
pub mod visibility;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KestrelBehaviorKind {
    Callable,
    Conformances,
    Executable,
    FunctionData,
    Generics,
    ImportData,
    MemberAccess,
    Typed,
    TypeAliasTyped,
    Valued,
    Visibility,
}
