use std::sync::Arc;

use kestrel_span::Span;
use semantic_tree::behavior::Behavior;

use crate::{behavior::KestrelBehaviorKind, language::KestrelLanguage, ty::Ty};

/// TypedBehavior represents the type information for a symbol or value
///
/// This behavior is used for:
/// - Class symbols (to represent their type)
/// - Function parameters and return types
/// - Variable declarations
/// - Any other construct that has an associated type
#[derive(Debug, Clone)]
pub struct TypedBehavior {
    /// The type associated with this symbol/value
    ty: Ty,
    /// The span where the type annotation appears in source code
    ty_span: Span,
}

impl Behavior<KestrelLanguage> for TypedBehavior {
    fn kind(&self) -> KestrelBehaviorKind {
        KestrelBehaviorKind::Typed
    }
}

impl TypedBehavior {
    /// Create a new TypedBehavior with the given type and span
    pub fn new(ty: Ty, ty_span: Span) -> Self {
        TypedBehavior { ty, ty_span }
    }

    /// Get the type
    pub fn ty(&self) -> &Ty {
        &self.ty
    }

    /// Get the span where the type annotation appears
    pub fn ty_span(&self) -> &Span {
        &self.ty_span
    }

    /// Get a mutable reference to the type
    /// This is useful during semantic analysis when resolving types
    pub fn ty_mut(&mut self) -> &mut Ty {
        &mut self.ty
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typed_behavior_unit() {
        let ty = Ty::unit(0..2);
        let behavior = TypedBehavior::new(ty, 0..2);

        assert!(behavior.ty().is_unit());
        assert_eq!(behavior.ty_span(), &(0..2));
    }

    #[test]
    fn test_typed_behavior_path() {
        let ty = Ty::path(vec!["Int".to_string()], 5..8);
        let behavior = TypedBehavior::new(ty, 5..8);

        assert!(behavior.ty().is_path());
        let segments = behavior.ty().as_path().unwrap();
        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0], "Int");
    }

    #[test]
    fn test_typed_behavior_function() {
        let param1 = Ty::path(vec!["Int".to_string()], 1..4);
        let param2 = Ty::path(vec!["String".to_string()], 6..12);
        let return_ty = Ty::path(vec!["Bool".to_string()], 17..21);

        let fn_ty = Ty::function(vec![param1, param2], return_ty, 0..21);
        let behavior = TypedBehavior::new(fn_ty, 0..21);

        assert!(behavior.ty().is_function());

        let (params, ret) = behavior.ty().as_function().unwrap();
        assert_eq!(params.len(), 2);
        assert!(ret.is_path());
    }
}
