mod kind;

pub use kind::TyKind;

use crate::symbol::class::ClassSymbol;
use crate::symbol::type_alias::TypeAliasSymbol;
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
        Self::new(TyKind::Path(segments), span)
    }

    /// Create a class type (resolved)
    pub fn class(class_symbol: Arc<ClassSymbol>, span: Span) -> Self {
        Self::new(TyKind::Class(class_symbol), span)
    }

    /// Create a type alias type
    pub fn type_alias(type_alias_symbol: Arc<TypeAliasSymbol>, span: Span) -> Self {
        Self::new(TyKind::TypeAlias(type_alias_symbol), span)
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
        matches!(self.kind, TyKind::Path(_))
    }

    /// Check if this is a class type (resolved)
    pub fn is_class(&self) -> bool {
        matches!(self.kind, TyKind::Class(_))
    }

    /// Check if this is a type alias type
    pub fn is_type_alias(&self) -> bool {
        matches!(self.kind, TyKind::TypeAlias(_))
    }

    // === Accessor methods ===

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
            TyKind::Path(segments) => Some(segments),
            _ => None,
        }
    }

    /// Get class symbol if this is a class type
    pub fn as_class(&self) -> Option<&Arc<ClassSymbol>> {
        match &self.kind {
            TyKind::Class(symbol) => Some(symbol),
            _ => None,
        }
    }

    /// Get type alias symbol if this is a type alias type
    pub fn as_type_alias(&self) -> Option<&Arc<TypeAliasSymbol>> {
        match &self.kind {
            TyKind::TypeAlias(symbol) => Some(symbol),
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
        assert!(!ty.is_class());
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
        assert!(!ty.is_class());
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
    fn test_class_type() {
        // For this test, we just verify the type checking works
        // We'll create a minimal path type and verify class type methods
        let ty = Ty::path(vec!["MyClass".to_string()], 0..7);

        // Verify it's a path type (unresolved)
        assert!(ty.is_path());
        assert!(!ty.is_class());

        let segments = ty.as_path().unwrap();
        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0], "MyClass");

        // Note: Testing actual class type creation would require
        // creating a ClassSymbol with all dependencies, which is
        // better tested in integration tests
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
