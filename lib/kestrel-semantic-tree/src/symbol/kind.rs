use semantic_tree::language::SymbolKind;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum KestrelSymbolKind {
    Class,
    Import,
    Module,
    SourceFile,
    TypeAlias,
}

impl SymbolKind for KestrelSymbolKind {
    /// Returns true if this symbol kind is transparent for name resolution.
    ///
    /// Transparent symbols (like SourceFile) are not directly visible in name lookups;
    /// instead, their children are surfaced to the parent scope.
    fn is_transparent(&self) -> bool {
        matches!(self, KestrelSymbolKind::SourceFile)
    }
}
