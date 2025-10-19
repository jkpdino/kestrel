use semantic_tree::language::Language;

use crate::{behavior::KestrelBehaviorKind, symbol::kind::KestrelSymbolKind};

#[derive(Debug)]
pub struct KestrelLanguage;

impl Language for KestrelLanguage {
    type BehaviorKind = KestrelBehaviorKind;

    type SymbolKind = KestrelSymbolKind;
}
