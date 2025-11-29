use crate::ty::Ty;
use crate::symbol::class::ClassSymbol;
use crate::symbol::protocol::ProtocolSymbol;
use crate::symbol::r#struct::StructSymbol;
use crate::symbol::type_alias::TypeAliasSymbol;
use std::sync::Arc;

/// Integer bit widths
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntBits {
    I8,
    I16,
    I32,
    I64,
}

/// Float bit widths
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatBits {
    F32,
    F64,
}

/// Represents the kind of a semantic type
/// These are resolved types after semantic analysis
#[derive(Debug, Clone)]
pub enum TyKind {
    /// Unit type: ()
    Unit,

    /// Never type: !
    Never,

    /// Integer type with bit width
    Int(IntBits),

    /// Float type with bit width
    Float(FloatBits),

    /// Boolean type
    Bool,

    /// String type
    String,

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

    /// Protocol type (resolved)
    /// This is a reference to a protocol symbol
    Protocol(Arc<ProtocolSymbol>),

    /// Struct type (resolved)
    /// This is a reference to a struct symbol
    Struct(Arc<StructSymbol>),

    /// Type alias type
    /// This is a reference to a type alias symbol
    /// During type resolution, this should be replaced with the resolved aliased type
    TypeAlias(Arc<TypeAliasSymbol>),
}
