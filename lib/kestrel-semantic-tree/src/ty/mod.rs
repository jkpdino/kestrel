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

    // === Constructors for each type variant ===

    /// Create a unit type: ()
    pub fn unit(span: Span) -> Self {
        Self::new(TyKind::Unit, span)
    }

    /// Create a never type: !
    pub fn never(span: Span) -> Self {
        Self::new(TyKind::Never, span)
    }

    /// Create an integer type with the given bit width
    pub fn int(bits: IntBits, span: Span) -> Self {
        Self::new(TyKind::Int(bits), span)
    }

    /// Create a float type with the given bit width
    pub fn float(bits: FloatBits, span: Span) -> Self {
        Self::new(TyKind::Float(bits), span)
    }

    /// Create a boolean type
    pub fn bool(span: Span) -> Self {
        Self::new(TyKind::Bool, span)
    }

    /// Create a string type
    pub fn string(span: Span) -> Self {
        Self::new(TyKind::String, span)
    }

    /// Create a tuple type: (T1, T2, ...)
    pub fn tuple(elements: Vec<Ty>, span: Span) -> Self {
        Self::new(TyKind::Tuple(elements), span)
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

    /// Create a path type (unresolved): A.B.C
    pub fn path(segments: Vec<String>, span: Span) -> Self {
        Self::new(TyKind::Path(segments, vec![]), span)
    }

    /// Create a generic path type (unresolved): A.B.C[T1, T2]
    pub fn generic_path(segments: Vec<String>, type_args: Vec<Ty>, span: Span) -> Self {
        Self::new(TyKind::Path(segments, type_args), span)
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

    // === Type checking methods ===

    /// Check if this is a unit type
    pub fn is_unit(&self) -> bool {
        matches!(self.kind, TyKind::Unit)
    }

    /// Check if this is a never type
    pub fn is_never(&self) -> bool {
        matches!(self.kind, TyKind::Never)
    }

    /// Check if this is an integer type
    pub fn is_int(&self) -> bool {
        matches!(self.kind, TyKind::Int(_))
    }

    /// Check if this is a float type
    pub fn is_float(&self) -> bool {
        matches!(self.kind, TyKind::Float(_))
    }

    /// Check if this is a boolean type
    pub fn is_bool(&self) -> bool {
        matches!(self.kind, TyKind::Bool)
    }

    /// Check if this is a string type
    pub fn is_string(&self) -> bool {
        matches!(self.kind, TyKind::String)
    }

    /// Check if this is a tuple type
    pub fn is_tuple(&self) -> bool {
        matches!(self.kind, TyKind::Tuple(_))
    }

    /// Check if this is a function type
    pub fn is_function(&self) -> bool {
        matches!(self.kind, TyKind::Function { .. })
    }

    /// Check if this is a path type (unresolved)
    pub fn is_path(&self) -> bool {
        matches!(self.kind, TyKind::Path(_, _))
    }

    /// Check if this is a type parameter type
    pub fn is_type_parameter(&self) -> bool {
        matches!(self.kind, TyKind::TypeParameter(_))
    }

    /// Check if this is a protocol type (resolved)
    pub fn is_protocol(&self) -> bool {
        matches!(self.kind, TyKind::Protocol { .. })
    }

    /// Check if this is a struct type (resolved)
    pub fn is_struct(&self) -> bool {
        matches!(self.kind, TyKind::Struct { .. })
    }

    /// Check if this is a type alias type
    pub fn is_type_alias(&self) -> bool {
        matches!(self.kind, TyKind::TypeAlias { .. })
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

    /// Get function parameters and return type if this is a function type
    pub fn as_function(&self) -> Option<(&Vec<Ty>, &Ty)> {
        match &self.kind {
            TyKind::Function { params, return_type } => Some((params, return_type)),
            _ => None,
        }
    }

    /// Get path segments if this is a path type
    pub fn as_path(&self) -> Option<&Vec<String>> {
        match &self.kind {
            TyKind::Path(segments, _) => Some(segments),
            _ => None,
        }
    }

    /// Get path segments and type arguments if this is a path type
    pub fn as_path_with_args(&self) -> Option<(&Vec<String>, &Vec<Ty>)> {
        match &self.kind {
            TyKind::Path(segments, args) => Some((segments, args)),
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
        assert!(!ty.is_path());
        assert!(!ty.is_type_alias());
    }

    #[test]
    fn test_never_type() {
        let ty = Ty::never(0..1);
        assert!(!ty.is_unit());
        assert!(ty.is_never());
        assert!(!ty.is_tuple());
        assert!(!ty.is_function());
        assert!(!ty.is_path());
        assert!(!ty.is_type_alias());
    }

    #[test]
    fn test_tuple_type() {
        let elem1 = Ty::unit(0..2);
        let elem2 = Ty::never(3..4);
        let ty = Ty::tuple(vec![elem1, elem2], 0..5);

        assert!(ty.is_tuple());
        assert!(!ty.is_unit());

        let elements = ty.as_tuple().unwrap();
        assert_eq!(elements.len(), 2);
        assert!(elements[0].is_unit());
        assert!(elements[1].is_never());
    }

    #[test]
    fn test_function_type() {
        let param1 = Ty::unit(0..2);
        let param2 = Ty::never(4..5);
        let return_ty = Ty::unit(10..12);

        let ty = Ty::function(vec![param1, param2], return_ty, 0..12);

        assert!(ty.is_function());
        assert!(!ty.is_unit());

        let (params, ret) = ty.as_function().unwrap();
        assert_eq!(params.len(), 2);
        assert!(params[0].is_unit());
        assert!(params[1].is_never());
        assert!(ret.is_unit());
    }

    #[test]
    fn test_path_type() {
        let ty = Ty::path(vec!["A".to_string(), "B".to_string(), "C".to_string()], 0..5);

        assert!(ty.is_path());
        assert!(!ty.is_unit());

        let segments = ty.as_path().unwrap();
        assert_eq!(segments.len(), 3);
        assert_eq!(segments[0], "A");
        assert_eq!(segments[1], "B");
        assert_eq!(segments[2], "C");
    }

    #[test]
    fn test_nested_types() {
        // Create a function type with tuple parameters: ((), !) -> ()
        let tuple_param = Ty::tuple(vec![Ty::unit(1..3), Ty::never(5..6)], 0..7);
        let return_ty = Ty::unit(12..14);

        let fn_ty = Ty::function(vec![tuple_param], return_ty, 0..14);

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
