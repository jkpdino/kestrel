pub trait Language {
    type BehaviorKind;
    type SymbolKind: Copy + Clone;
}
