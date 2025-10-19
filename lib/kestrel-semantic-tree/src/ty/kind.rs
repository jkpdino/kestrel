use crate::ty::Ty;
use crate::symbol::class::ClassSymbol;
use std::sync::Arc;

/// Represents the kind of a semantic type
/// These are resolved types after semantic analysis
#[derive(Debug, Clone)]
pub enum TyKind {
    /// Unit type: ()
    Unit,

    /// Never type: !
    Never,

    /// Tuple type: (T1, T2, ...)
    Tuple(Vec<Ty>),

    /// Function type: (P1, P2, ...) -> R
    Function {
        params: Vec<Ty>,
        return_type: Box<Ty>,
    },

    /// Path type (unresolved)
    /// This represents a path like A.B.C that hasn't been resolved yet
    /// During semantic analysis, this should be resolved to a concrete type
    Path(Vec<String>),

    /// Class type (resolved)
    /// This is a reference to a class symbol
    Class(Arc<ClassSymbol>),
}
