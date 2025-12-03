//! Member access behavior for symbols that can be accessed as members.
//!
//! This behavior is attached to symbols (like fields) that can be accessed
//! through the dot operator on a parent expression (e.g., `obj.field`).

use kestrel_span::Span;
use semantic_tree::behavior::Behavior;

use crate::{behavior::KestrelBehaviorKind, expr::Expression, language::KestrelLanguage, ty::Ty};

/// Behavior for symbols that can be accessed as members of a parent expression.
///
/// When you write `obj.field`, the field symbol has a `MemberAccessBehavior`
/// that knows how to produce the resulting expression given the parent `obj`.
#[derive(Debug, Clone)]
pub struct MemberAccessBehavior {
    /// The name of the member (for producing FieldAccess expressions)
    member_name: String,
    /// The type of the member when accessed
    member_type: Ty,
}

impl Behavior<KestrelLanguage> for MemberAccessBehavior {
    fn kind(&self) -> KestrelBehaviorKind {
        KestrelBehaviorKind::MemberAccess
    }
}

impl MemberAccessBehavior {
    /// Create a new MemberAccessBehavior for a field
    pub fn new(member_name: String, member_type: Ty) -> Self {
        MemberAccessBehavior {
            member_name,
            member_type,
        }
    }

    /// Get the member name
    pub fn member_name(&self) -> &str {
        &self.member_name
    }

    /// Get the member type
    pub fn member_type(&self) -> &Ty {
        &self.member_type
    }

    /// Produce an expression for accessing this member on the given parent expression.
    ///
    /// For a field, this produces `Expression::field_access(parent, field_name, field_type, span)`.
    pub fn access(&self, parent: Expression, span: Span) -> Expression {
        Expression::field_access(parent, self.member_name.clone(), self.member_type.clone(), span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ty::IntBits;

    #[test]
    fn test_member_access_field() {
        let field_ty = Ty::int(IntBits::I64, 0..3);
        let behavior = MemberAccessBehavior::new("x".to_string(), field_ty.clone());

        assert_eq!(behavior.member_name(), "x");
        assert!(behavior.member_type().is_int());

        // Create a fake parent expression
        let parent = Expression::integer(42, 0..2);
        let result = behavior.access(parent, 0..4);

        // Result should be a field access
        match &result.kind {
            crate::expr::ExprKind::FieldAccess { field, .. } => {
                assert_eq!(field, "x");
            }
            _ => panic!("Expected FieldAccess"),
        }
    }
}
