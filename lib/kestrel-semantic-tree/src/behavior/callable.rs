use std::hash::{Hash, Hasher};

use kestrel_span::{Name, Span};
use semantic_tree::behavior::Behavior;
use semantic_tree::symbol::Symbol;

use crate::{behavior::KestrelBehaviorKind, language::KestrelLanguage, ty::Ty};

/// Represents a function parameter with optional label for overload resolution.
///
/// Parameters support Swift-style labeled arguments:
/// - `label` is the external name used by callers (optional)
/// - `bind_name` is the internal name used in the function body
///
/// Examples:
/// - `x: Int` -> label=None, bind_name="x"
/// - `with x: Int` -> label="with", bind_name="x"
#[derive(Debug, Clone)]
pub struct CallableParameter {
    /// Optional external label for callers
    pub label: Option<Name>,
    /// Internal binding name used in function body
    pub bind_name: Name,
    /// The parameter's type
    pub ty: Ty,
}

impl CallableParameter {
    /// Create a new parameter without a label
    pub fn new(bind_name: Name, ty: Ty) -> Self {
        Self {
            label: None,
            bind_name,
            ty,
        }
    }

    /// Create a new parameter with a label
    pub fn with_label(label: Name, bind_name: Name, ty: Ty) -> Self {
        Self {
            label: Some(label),
            bind_name,
            ty,
        }
    }

    /// Get the external label if present.
    ///
    /// Returns None if the parameter has no explicit label (unlabeled parameter).
    /// Unlabeled parameters are called positionally without a label.
    pub fn external_label(&self) -> Option<&str> {
        self.label.as_ref().map(|l| l.value.as_str())
    }

    /// Get the internal binding name
    pub fn internal_name(&self) -> &str {
        &self.bind_name.value
    }

    /// Check if this parameter has an explicit label
    pub fn has_label(&self) -> bool {
        self.label.is_some()
    }
}

/// Uniquely identifies a callable for overload resolution and duplicate detection.
///
/// Two callables with the same signature are considered duplicates and will
/// cause a compilation error.
///
/// The signature consists of:
/// - The callable's name
/// - The labels for each parameter (None = unlabeled positional parameter)
/// - The parameter types (for type-based overloading)
#[derive(Debug, Clone)]
pub struct CallableSignature {
    /// Name of the callable
    pub name: String,
    /// Labels for each parameter (None = unlabeled positional parameter)
    pub labels: Vec<Option<String>>,
    /// Parameter types for type-based overloading
    pub param_types: Vec<SignatureType>,
}

/// Simplified type representation for signature comparison.
///
/// This is used instead of full `Ty` because:
/// 1. We need Hash + Eq for HashMap-based duplicate detection
/// 2. We only care about structural equality, not spans
/// 3. Unresolved paths are compared by name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignatureType {
    /// Unit type ()
    Unit,
    /// Never type !
    Never,
    /// Boolean
    Bool,
    /// Integer (we ignore bit width for now - could refine later)
    Int,
    /// Float (we ignore bit width for now)
    Float,
    /// String
    String,
    /// Tuple of types
    Tuple(Vec<SignatureType>),
    /// Function type
    Function {
        params: Vec<SignatureType>,
        return_type: Box<SignatureType>,
    },
    /// Named type (unresolved path or resolved class/struct)
    Named(Vec<String>),
    /// Unknown/error type
    Unknown,
}

impl SignatureType {
    /// Convert a Ty to a SignatureType for comparison
    pub fn from_ty(ty: &Ty) -> Self {
        use crate::ty::TyKind;

        match ty.kind() {
            TyKind::Unit => SignatureType::Unit,
            TyKind::Never => SignatureType::Never,
            TyKind::Bool => SignatureType::Bool,
            TyKind::Int(_) => SignatureType::Int,
            TyKind::Float(_) => SignatureType::Float,
            TyKind::String => SignatureType::String,
            TyKind::Tuple(elements) => {
                SignatureType::Tuple(elements.iter().map(SignatureType::from_ty).collect())
            }
            TyKind::Function {
                params,
                return_type,
            } => SignatureType::Function {
                params: params.iter().map(SignatureType::from_ty).collect(),
                return_type: Box::new(SignatureType::from_ty(return_type)),
            },
            TyKind::Path(segments) => SignatureType::Named(segments.clone()),
            TyKind::Class(class) => {
                // Use the class name as the type name
                SignatureType::Named(vec![class.metadata().name().value.clone()])
            }
            TyKind::Struct(s) => {
                SignatureType::Named(vec![s.metadata().name().value.clone()])
            }
            TyKind::TypeAlias(alias) => {
                // For type aliases, use the alias name
                // (could also resolve to underlying type)
                SignatureType::Named(vec![alias.metadata().name().value.clone()])
            }
        }
    }
}

impl PartialEq for CallableSignature {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.labels == other.labels
            && self.param_types == other.param_types
    }
}

impl Eq for CallableSignature {}

impl Hash for CallableSignature {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.labels.hash(state);
        self.param_types.hash(state);
    }
}

impl CallableSignature {
    /// Create a new signature
    pub fn new(name: String, labels: Vec<Option<String>>, param_types: Vec<SignatureType>) -> Self {
        Self {
            name,
            labels,
            param_types,
        }
    }

    /// Get the arity (number of parameters)
    pub fn arity(&self) -> usize {
        self.param_types.len()
    }

    /// Format the signature for display in error messages
    pub fn display(&self) -> String {
        let params: Vec<String> = self
            .labels
            .iter()
            .zip(self.param_types.iter())
            .map(|(label, ty)| {
                match label {
                    Some(l) => format!("{}: {:?}", l, ty),
                    None => format!("_: {:?}", ty),
                }
            })
            .collect();

        format!("{}({})", self.name, params.join(", "))
    }
}

/// CallableBehavior represents callable semantics that can be attached to symbols.
///
/// This behavior is used for:
/// - Functions (standalone and methods)
/// - Initializers (future)
/// - Closures (future)
///
/// It provides:
/// - Parameter information with labels for overload resolution
/// - Return type for type checking
/// - Signature generation for duplicate detection
#[derive(Debug, Clone)]
pub struct CallableBehavior {
    /// The callable's parameters
    parameters: Vec<CallableParameter>,
    /// The return type
    return_type: Ty,
    /// The span covering the entire callable declaration
    span: Span,
}

impl Behavior<KestrelLanguage> for CallableBehavior {
    fn kind(&self) -> KestrelBehaviorKind {
        KestrelBehaviorKind::Callable
    }
}

impl CallableBehavior {
    /// Create a new CallableBehavior
    pub fn new(parameters: Vec<CallableParameter>, return_type: Ty, span: Span) -> Self {
        Self {
            parameters,
            return_type,
            span,
        }
    }

    /// Get the parameters
    pub fn parameters(&self) -> &[CallableParameter] {
        &self.parameters
    }

    /// Get the return type
    pub fn return_type(&self) -> &Ty {
        &self.return_type
    }

    /// Get the span
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Get the arity (number of parameters)
    pub fn arity(&self) -> usize {
        self.parameters.len()
    }

    /// Generate a signature for this callable with the given name.
    ///
    /// The signature is used for:
    /// - Duplicate detection (same signature = error)
    /// - Overload resolution (different signatures = valid overloads)
    pub fn signature(&self, name: &str) -> CallableSignature {
        let labels: Vec<Option<String>> = self
            .parameters
            .iter()
            .map(|p| p.external_label().map(|s| s.to_string()))
            .collect();

        let param_types: Vec<SignatureType> = self
            .parameters
            .iter()
            .map(|p| SignatureType::from_ty(&p.ty))
            .collect();

        CallableSignature::new(name.to_string(), labels, param_types)
    }

    /// Get the function type representation of this callable
    pub fn function_type(&self) -> Ty {
        let param_types: Vec<Ty> = self.parameters.iter().map(|p| p.ty.clone()).collect();
        Ty::function(param_types, self.return_type.clone(), self.span.clone())
    }

    /// Get parameter labels for display/debugging
    pub fn parameter_labels(&self) -> Vec<Option<&str>> {
        self.parameters
            .iter()
            .map(|p| p.external_label())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_span::Spanned;

    fn make_name(s: &str) -> Name {
        Spanned::new(s.to_string(), 0..s.len())
    }

    #[test]
    fn test_signature_equality_same_unlabeled() {
        // Two unlabeled parameters with same types = same signature
        let sig1 = CallableSignature::new(
            "add".to_string(),
            vec![None, None],
            vec![SignatureType::Int, SignatureType::Int],
        );
        let sig2 = CallableSignature::new(
            "add".to_string(),
            vec![None, None],
            vec![SignatureType::Int, SignatureType::Int],
        );

        assert_eq!(sig1, sig2);
    }

    #[test]
    fn test_signature_equality_same_labeled() {
        // Two labeled parameters with same labels and types = same signature
        let sig1 = CallableSignature::new(
            "add".to_string(),
            vec![Some("x".to_string()), Some("y".to_string())],
            vec![SignatureType::Int, SignatureType::Int],
        );
        let sig2 = CallableSignature::new(
            "add".to_string(),
            vec![Some("x".to_string()), Some("y".to_string())],
            vec![SignatureType::Int, SignatureType::Int],
        );

        assert_eq!(sig1, sig2);
    }

    #[test]
    fn test_signature_different_labels() {
        let sig1 = CallableSignature::new(
            "greet".to_string(),
            vec![Some("with".to_string())],
            vec![SignatureType::Named(vec!["String".to_string()])],
        );
        let sig2 = CallableSignature::new(
            "greet".to_string(),
            vec![Some("using".to_string())],
            vec![SignatureType::Named(vec!["String".to_string()])],
        );

        assert_ne!(sig1, sig2); // Different labels = different signatures
    }

    #[test]
    fn test_signature_labeled_vs_unlabeled() {
        // Labeled vs unlabeled = different signatures
        let sig1 = CallableSignature::new(
            "foo".to_string(),
            vec![Some("x".to_string())],
            vec![SignatureType::Int],
        );
        let sig2 = CallableSignature::new(
            "foo".to_string(),
            vec![None],
            vec![SignatureType::Int],
        );

        assert_ne!(sig1, sig2); // Labeled vs unlabeled = different signatures
    }

    #[test]
    fn test_signature_different_types() {
        let sig1 = CallableSignature::new(
            "add".to_string(),
            vec![None, None],
            vec![SignatureType::Int, SignatureType::Int],
        );
        let sig2 = CallableSignature::new(
            "add".to_string(),
            vec![None, None],
            vec![SignatureType::Float, SignatureType::Float],
        );

        assert_ne!(sig1, sig2); // Different types = different signatures
    }

    #[test]
    fn test_signature_different_arity() {
        let sig1 = CallableSignature::new(
            "add".to_string(),
            vec![None],
            vec![SignatureType::Int],
        );
        let sig2 = CallableSignature::new(
            "add".to_string(),
            vec![None, None],
            vec![SignatureType::Int, SignatureType::Int],
        );

        assert_ne!(sig1, sig2); // Different arity = different signatures
    }

    #[test]
    fn test_callable_behavior_signature_unlabeled() {
        // Unlabeled parameters have None for labels
        let params = vec![
            CallableParameter::new(make_name("x"), Ty::path(vec!["Int".to_string()], 0..3)),
            CallableParameter::new(make_name("y"), Ty::path(vec!["Int".to_string()], 5..8)),
        ];
        let return_ty = Ty::path(vec!["Int".to_string()], 13..16);
        let behavior = CallableBehavior::new(params, return_ty, 0..20);

        let sig = behavior.signature("add");

        assert_eq!(sig.name, "add");
        // Unlabeled params have None for labels
        assert_eq!(sig.labels, vec![None, None]);
        assert_eq!(sig.arity(), 2);
    }

    #[test]
    fn test_callable_with_labels() {
        let params = vec![CallableParameter::with_label(
            make_name("with"),
            make_name("name"),
            Ty::path(vec!["String".to_string()], 0..6),
        )];
        let return_ty = Ty::unit(10..12);
        let behavior = CallableBehavior::new(params, return_ty, 0..15);

        let sig = behavior.signature("greet");

        assert_eq!(sig.name, "greet");
        assert_eq!(sig.labels, vec![Some("with".to_string())]); // Uses label, not bind_name
    }
}
