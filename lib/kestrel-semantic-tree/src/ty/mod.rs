mod kind;
pub mod substitutions;
pub mod where_clause;

pub use kind::{FloatBits, IntBits, TyKind};
pub use substitutions::Substitutions;
pub use where_clause::{Constraint, WhereClause};

use crate::symbol::protocol::ProtocolSymbol;
use crate::symbol::r#struct::StructSymbol;
use crate::symbol::type_alias::TypeAliasSymbol;
use crate::symbol::type_parameter::TypeParameterSymbol;
use kestrel_span::Span;
use std::sync::Arc;

/// Represents a semantic type with its kind and source location
#[derive(Debug, Clone)]
pub struct Ty {
    kind: TyKind,
    span: Span,
}

/// Generate simple type constructors that take only a span
macro_rules! simple_constructor {
    ($($(#[$meta:meta])* $name:ident => $variant:expr),* $(,)?) => {
        $(
            $(#[$meta])*
            pub fn $name(span: Span) -> Self {
                Self::new($variant, span)
            }
        )*
    };
}

/// Generate type checking methods (is_* methods)
macro_rules! is_type {
    ($($(#[$meta:meta])* $name:ident => $pattern:pat),* $(,)?) => {
        $(
            $(#[$meta])*
            pub fn $name(&self) -> bool {
                matches!(self.kind, $pattern)
            }
        )*
    };
}

impl Ty {
    /// Create a new type with the given kind and span
    pub fn new(kind: TyKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Get the kind of this type
    pub fn kind(&self) -> &TyKind {
        &self.kind
    }

    /// Get the span of this type
    pub fn span(&self) -> &Span {
        &self.span
    }

    // === Simple constructors (generated) ===
    simple_constructor! {
        /// Create a unit type: ()
        unit => TyKind::Unit,
        /// Create a never type: !
        never => TyKind::Never,
        /// Create a boolean type
        bool => TyKind::Bool,
        /// Create a string type
        string => TyKind::String,
        /// Create an error type (poison value)
        error => TyKind::Error,
        /// Create a Self type reference
        self_type => TyKind::SelfType,
        /// Create an inferred type placeholder
        inferred => TyKind::Inferred,
    }

    // === Parameterized constructors ===

    /// Create an integer type with the given bit width
    pub fn int(bits: IntBits, span: Span) -> Self {
        Self::new(TyKind::Int(bits), span)
    }

    /// Create a float type with the given bit width
    pub fn float(bits: FloatBits, span: Span) -> Self {
        Self::new(TyKind::Float(bits), span)
    }

    /// Create a tuple type: (T1, T2, ...)
    pub fn tuple(elements: Vec<Ty>, span: Span) -> Self {
        Self::new(TyKind::Tuple(elements), span)
    }

    /// Create an array type: [T]
    pub fn array(element_type: Ty, span: Span) -> Self {
        Self::new(TyKind::Array(Box::new(element_type)), span)
    }

    /// Create a function type: (P1, P2, ...) -> R
    pub fn function(params: Vec<Ty>, return_type: Ty, span: Span) -> Self {
        Self::new(
            TyKind::Function {
                params,
                return_type: Box::new(return_type),
            },
            span,
        )
    }

    /// Create a type parameter reference
    pub fn type_parameter(param_symbol: Arc<TypeParameterSymbol>, span: Span) -> Self {
        Self::new(TyKind::TypeParameter(param_symbol), span)
    }

    /// Create a protocol type (resolved) with no type arguments
    pub fn protocol(protocol_symbol: Arc<ProtocolSymbol>, span: Span) -> Self {
        Self::new(
            TyKind::Protocol {
                symbol: protocol_symbol,
                substitutions: Substitutions::new(),
            },
            span,
        )
    }

    /// Create a generic protocol type (resolved) with type arguments
    pub fn generic_protocol(
        protocol_symbol: Arc<ProtocolSymbol>,
        substitutions: Substitutions,
        span: Span,
    ) -> Self {
        Self::new(
            TyKind::Protocol {
                symbol: protocol_symbol,
                substitutions,
            },
            span,
        )
    }

    /// Create a struct type (resolved) with no type arguments
    pub fn r#struct(struct_symbol: Arc<StructSymbol>, span: Span) -> Self {
        Self::new(
            TyKind::Struct {
                symbol: struct_symbol,
                substitutions: Substitutions::new(),
            },
            span,
        )
    }

    /// Create a generic struct type (resolved) with type arguments
    pub fn generic_struct(
        struct_symbol: Arc<StructSymbol>,
        substitutions: Substitutions,
        span: Span,
    ) -> Self {
        Self::new(
            TyKind::Struct {
                symbol: struct_symbol,
                substitutions,
            },
            span,
        )
    }

    /// Create a type alias type with no type arguments
    pub fn type_alias(type_alias_symbol: Arc<TypeAliasSymbol>, span: Span) -> Self {
        Self::new(
            TyKind::TypeAlias {
                symbol: type_alias_symbol,
                substitutions: Substitutions::new(),
            },
            span,
        )
    }

    /// Create a generic type alias type with type arguments
    pub fn generic_type_alias(
        type_alias_symbol: Arc<TypeAliasSymbol>,
        substitutions: Substitutions,
        span: Span,
    ) -> Self {
        Self::new(
            TyKind::TypeAlias {
                symbol: type_alias_symbol,
                substitutions,
            },
            span,
        )
    }

    // === Type checking methods (generated) ===
    is_type! {
        /// Check if this is a unit type
        is_unit => TyKind::Unit,
        /// Check if this is a never type
        is_never => TyKind::Never,
        /// Check if this is an integer type
        is_int => TyKind::Int(_),
        /// Check if this is a float type
        is_float => TyKind::Float(_),
        /// Check if this is a boolean type
        is_bool => TyKind::Bool,
        /// Check if this is a string type
        is_string => TyKind::String,
        /// Check if this is a tuple type
        is_tuple => TyKind::Tuple(_),
        /// Check if this is an array type
        is_array => TyKind::Array(_),
        /// Check if this is a function type
        is_function => TyKind::Function { .. },
        /// Check if this is an error type
        is_error => TyKind::Error,
        /// Check if this is a Self type reference
        is_self_type => TyKind::SelfType,
        /// Check if this is an inferred type
        is_inferred => TyKind::Inferred,
        /// Check if this is a type parameter type
        is_type_parameter => TyKind::TypeParameter(_),
        /// Check if this is a protocol type (resolved)
        is_protocol => TyKind::Protocol { .. },
        /// Check if this is a struct type (resolved)
        is_struct => TyKind::Struct { .. },
        /// Check if this is a type alias type
        is_type_alias => TyKind::TypeAlias { .. },
    }

    // === Accessor methods ===

    /// Get integer bit width if this is an integer type
    pub fn as_int(&self) -> Option<IntBits> {
        match &self.kind {
            TyKind::Int(bits) => Some(*bits),
            _ => None,
        }
    }

    /// Get float bit width if this is a float type
    pub fn as_float(&self) -> Option<FloatBits> {
        match &self.kind {
            TyKind::Float(bits) => Some(*bits),
            _ => None,
        }
    }

    /// Get tuple elements if this is a tuple type
    pub fn as_tuple(&self) -> Option<&Vec<Ty>> {
        match &self.kind {
            TyKind::Tuple(elements) => Some(elements),
            _ => None,
        }
    }

    /// Get array element type if this is an array type
    pub fn as_array(&self) -> Option<&Ty> {
        match &self.kind {
            TyKind::Array(element_type) => Some(element_type),
            _ => None,
        }
    }

    /// Get function parameters and return type if this is a function type
    pub fn as_function(&self) -> Option<(&Vec<Ty>, &Ty)> {
        match &self.kind {
            TyKind::Function { params, return_type } => Some((params, return_type)),
            _ => None,
        }
    }

    /// Get type parameter symbol if this is a type parameter type
    pub fn as_type_parameter(&self) -> Option<&Arc<TypeParameterSymbol>> {
        match &self.kind {
            TyKind::TypeParameter(symbol) => Some(symbol),
            _ => None,
        }
    }

    /// Get protocol symbol if this is a protocol type
    pub fn as_protocol(&self) -> Option<&Arc<ProtocolSymbol>> {
        match &self.kind {
            TyKind::Protocol { symbol, .. } => Some(symbol),
            _ => None,
        }
    }

    /// Get protocol symbol and substitutions if this is a protocol type
    pub fn as_protocol_with_subs(&self) -> Option<(&Arc<ProtocolSymbol>, &Substitutions)> {
        match &self.kind {
            TyKind::Protocol { symbol, substitutions } => Some((symbol, substitutions)),
            _ => None,
        }
    }

    /// Get struct symbol if this is a struct type
    pub fn as_struct(&self) -> Option<&Arc<StructSymbol>> {
        match &self.kind {
            TyKind::Struct { symbol, .. } => Some(symbol),
            _ => None,
        }
    }

    /// Get struct symbol and substitutions if this is a struct type
    pub fn as_struct_with_subs(&self) -> Option<(&Arc<StructSymbol>, &Substitutions)> {
        match &self.kind {
            TyKind::Struct { symbol, substitutions } => Some((symbol, substitutions)),
            _ => None,
        }
    }

    /// Get type alias symbol if this is a type alias type
    pub fn as_type_alias(&self) -> Option<&Arc<TypeAliasSymbol>> {
        match &self.kind {
            TyKind::TypeAlias { symbol, .. } => Some(symbol),
            _ => None,
        }
    }

    /// Get type alias symbol and substitutions if this is a type alias type
    pub fn as_type_alias_with_subs(&self) -> Option<(&Arc<TypeAliasSymbol>, &Substitutions)> {
        match &self.kind {
            TyKind::TypeAlias { symbol, substitutions } => Some((symbol, substitutions)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unit_type() {
        let ty = Ty::unit(0..2);
        assert!(ty.is_unit());
        assert!(!ty.is_never());
        assert!(!ty.is_tuple());
        assert!(!ty.is_function());
        assert!(!ty.is_error());
        assert!(!ty.is_type_alias());
    }

    #[test]
    fn test_never_type() {
        let ty = Ty::never(0..1);
        assert!(!ty.is_unit());
        assert!(ty.is_never());
    }

    #[test]
    fn test_error_type() {
        let ty = Ty::error(0..5);
        assert!(ty.is_error());
        assert!(!ty.is_unit());
    }

    #[test]
    fn test_self_type() {
        let ty = Ty::self_type(0..4);
        assert!(ty.is_self_type());
        assert!(!ty.is_unit());
    }

    #[test]
    fn test_inferred_type() {
        let ty = Ty::inferred(0..1);
        assert!(ty.is_inferred());
        assert!(!ty.is_unit());
    }

    #[test]
    fn test_tuple_type() {
        let ty = Ty::tuple(vec![Ty::unit(0..2), Ty::never(3..4)], 0..5);
        assert!(ty.is_tuple());
        assert!(!ty.is_unit());

        let elements = ty.as_tuple().unwrap();
        assert_eq!(elements.len(), 2);
        assert!(elements[0].is_unit());
        assert!(elements[1].is_never());
    }

    #[test]
    fn test_function_type() {
        let ty = Ty::function(
            vec![Ty::unit(0..2), Ty::never(4..5)],
            Ty::unit(10..12),
            0..12,
        );
        assert!(ty.is_function());
        assert!(!ty.is_unit());

        let (params, ret) = ty.as_function().unwrap();
        assert_eq!(params.len(), 2);
        assert!(params[0].is_unit());
        assert!(params[1].is_never());
        assert!(ret.is_unit());
    }

    #[test]
    fn test_nested_types() {
        let tuple_param = Ty::tuple(vec![Ty::unit(1..3), Ty::never(5..6)], 0..7);
        let fn_ty = Ty::function(vec![tuple_param], Ty::unit(12..14), 0..14);

        assert!(fn_ty.is_function());

        let (params, ret) = fn_ty.as_function().unwrap();
        assert_eq!(params.len(), 1);
        assert!(params[0].is_tuple());
        assert!(ret.is_unit());

        let tuple_elements = params[0].as_tuple().unwrap();
        assert_eq!(tuple_elements.len(), 2);
        assert!(tuple_elements[0].is_unit());
        assert!(tuple_elements[1].is_never());
    }
}
