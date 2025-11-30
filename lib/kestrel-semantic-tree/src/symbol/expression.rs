use std::sync::Arc;

use kestrel_span::{Name, Span};
use semantic_tree::symbol::{Symbol, SymbolMetadata, SymbolMetadataBuilder};

use crate::language::KestrelLanguage;
use crate::symbol::kind::KestrelSymbolKind;
use crate::ty::Ty;

/// Represents a literal value in an expression
#[derive(Debug, Clone)]
pub enum LiteralValue {
    /// Unit literal: ()
    Unit,
    /// Integer literal: 42, 0xFF, 0b1010, 0o17
    Integer(i64),
    /// Float literal: 3.14, 1.0e10
    Float(f64),
    /// String literal: "hello"
    String(String),
    /// Boolean literal: true, false
    Bool(bool),
}

/// Represents the kind of expression
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Literal expression (integer, float, string, bool, unit)
    Literal(LiteralValue),
    /// Array literal: [1, 2, 3]
    Array(Vec<Arc<ExpressionSymbol>>),
    /// Tuple literal: (1, 2, 3)
    Tuple(Vec<Arc<ExpressionSymbol>>),
    /// Grouping expression: (expr)
    Grouping(Arc<ExpressionSymbol>),
}

/// A symbol representing an expression in the semantic tree
#[derive(Debug)]
pub struct ExpressionSymbol {
    metadata: SymbolMetadata<KestrelLanguage>,
    expr_kind: ExprKind,
    ty: Option<Ty>,
}

impl Symbol<KestrelLanguage> for ExpressionSymbol {
    fn metadata(&self) -> &SymbolMetadata<KestrelLanguage> {
        &self.metadata
    }
}

impl ExpressionSymbol {
    /// Create a new ExpressionSymbol
    pub fn new(
        name: Name,
        span: Span,
        expr_kind: ExprKind,
        ty: Option<Ty>,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        let mut builder = SymbolMetadataBuilder::new(KestrelSymbolKind::Expression)
            .with_name(name.clone())
            .with_declaration_span(name.span.clone())
            .with_span(span);

        if let Some(p) = parent {
            builder = builder.with_parent(Arc::downgrade(&p));
        }

        ExpressionSymbol {
            metadata: builder.build(),
            expr_kind,
            ty,
        }
    }

    /// Create an integer literal expression
    pub fn integer(value: i64, span: Span, parent: Option<Arc<dyn Symbol<KestrelLanguage>>>) -> Self {
        let name = Name::new(value.to_string(), span.clone());
        Self::new(
            name,
            span.clone(),
            ExprKind::Literal(LiteralValue::Integer(value)),
            Some(Ty::int(crate::ty::IntBits::I64, span)),
            parent,
        )
    }

    /// Create a float literal expression
    pub fn float(value: f64, span: Span, parent: Option<Arc<dyn Symbol<KestrelLanguage>>>) -> Self {
        let name = Name::new(value.to_string(), span.clone());
        Self::new(
            name,
            span.clone(),
            ExprKind::Literal(LiteralValue::Float(value)),
            Some(Ty::float(crate::ty::FloatBits::F64, span)),
            parent,
        )
    }

    /// Create a string literal expression
    pub fn string(value: String, span: Span, parent: Option<Arc<dyn Symbol<KestrelLanguage>>>) -> Self {
        let name = Name::new(format!("\"{}\"", value), span.clone());
        Self::new(
            name,
            span.clone(),
            ExprKind::Literal(LiteralValue::String(value)),
            Some(Ty::string(span)),
            parent,
        )
    }

    /// Create a boolean literal expression
    pub fn bool(value: bool, span: Span, parent: Option<Arc<dyn Symbol<KestrelLanguage>>>) -> Self {
        let name = Name::new(value.to_string(), span.clone());
        Self::new(
            name,
            span.clone(),
            ExprKind::Literal(LiteralValue::Bool(value)),
            Some(Ty::bool(span)),
            parent,
        )
    }

    /// Create a unit literal expression
    pub fn unit(span: Span, parent: Option<Arc<dyn Symbol<KestrelLanguage>>>) -> Self {
        let name = Name::new("()".to_string(), span.clone());
        Self::new(
            name,
            span.clone(),
            ExprKind::Literal(LiteralValue::Unit),
            Some(Ty::unit(span)),
            parent,
        )
    }

    /// Create an array literal expression
    pub fn array(
        elements: Vec<Arc<ExpressionSymbol>>,
        span: Span,
        element_ty: Option<Ty>,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        let name = Name::new("[...]".to_string(), span.clone());
        let ty = element_ty.map(|et| Ty::array(et, span.clone()));
        Self::new(name, span, ExprKind::Array(elements), ty, parent)
    }

    /// Create a tuple literal expression
    pub fn tuple(
        elements: Vec<Arc<ExpressionSymbol>>,
        span: Span,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        let name = Name::new("(...)".to_string(), span.clone());
        let element_types: Vec<Ty> = elements
            .iter()
            .filter_map(|e| e.ty().cloned())
            .collect();
        let ty = if element_types.len() == elements.len() {
            Some(Ty::tuple(element_types, span.clone()))
        } else {
            None
        };
        Self::new(name, span, ExprKind::Tuple(elements), ty, parent)
    }

    /// Create a grouping expression
    pub fn grouping(
        inner: Arc<ExpressionSymbol>,
        span: Span,
        parent: Option<Arc<dyn Symbol<KestrelLanguage>>>,
    ) -> Self {
        let name = Name::new("(...)".to_string(), span.clone());
        let ty = inner.ty().cloned();
        Self::new(name, span, ExprKind::Grouping(inner), ty, parent)
    }

    /// Get the expression kind
    pub fn expr_kind(&self) -> &ExprKind {
        &self.expr_kind
    }

    /// Get the type of this expression (if known)
    pub fn ty(&self) -> Option<&Ty> {
        self.ty.as_ref()
    }

    /// Check if this is a literal expression
    pub fn is_literal(&self) -> bool {
        matches!(self.expr_kind, ExprKind::Literal(_))
    }

    /// Check if this is an array expression
    pub fn is_array(&self) -> bool {
        matches!(self.expr_kind, ExprKind::Array(_))
    }

    /// Check if this is a tuple expression
    pub fn is_tuple(&self) -> bool {
        matches!(self.expr_kind, ExprKind::Tuple(_))
    }

    /// Check if this is a grouping expression
    pub fn is_grouping(&self) -> bool {
        matches!(self.expr_kind, ExprKind::Grouping(_))
    }

    /// Get the literal value if this is a literal expression
    pub fn as_literal(&self) -> Option<&LiteralValue> {
        match &self.expr_kind {
            ExprKind::Literal(val) => Some(val),
            _ => None,
        }
    }

    /// Get array elements if this is an array expression
    pub fn as_array(&self) -> Option<&Vec<Arc<ExpressionSymbol>>> {
        match &self.expr_kind {
            ExprKind::Array(elements) => Some(elements),
            _ => None,
        }
    }

    /// Get tuple elements if this is a tuple expression
    pub fn as_tuple(&self) -> Option<&Vec<Arc<ExpressionSymbol>>> {
        match &self.expr_kind {
            ExprKind::Tuple(elements) => Some(elements),
            _ => None,
        }
    }

    /// Get inner expression if this is a grouping expression
    pub fn as_grouping(&self) -> Option<&Arc<ExpressionSymbol>> {
        match &self.expr_kind {
            ExprKind::Grouping(inner) => Some(inner),
            _ => None,
        }
    }
}
