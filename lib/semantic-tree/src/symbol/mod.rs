mod collection;
mod metadata;
mod table;

use std::fmt::Debug;

pub use collection::SymbolCollection;
pub use metadata::SymbolMetadata;
pub use metadata::SymbolMetadataBuilder;
pub use table::SymbolTable;

use crate::language::Language;

pub trait Symbol<L: Language>: Debug + Send + Sync {
    fn metadata(&self) -> &SymbolMetadata<L>;
}
