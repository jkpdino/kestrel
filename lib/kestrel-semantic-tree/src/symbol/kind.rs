#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum KestrelSymbolKind {
    Class,
    Import,
    Module,
    SourceFile,
    TypeAlias,
}
