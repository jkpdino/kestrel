//! Shared utilities for body resolution.
//!
//! This module contains helper functions used across multiple body resolution
//! modules, including type formatting, signature matching, and behavior lookups.

use std::sync::Arc;

use kestrel_semantic_tree::behavior::callable::CallableBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use kestrel_span::Span;
use kestrel_syntax_tree::SyntaxKind;
use semantic_tree::symbol::Symbol;

use super::context::BodyResolutionContext;

/// Check if a syntax kind is an expression kind
pub fn is_expression_kind(kind: SyntaxKind) -> bool {
    matches!(kind,
        SyntaxKind::Expression
        | SyntaxKind::ExprUnit
        | SyntaxKind::ExprInteger
        | SyntaxKind::ExprFloat
        | SyntaxKind::ExprString
        | SyntaxKind::ExprBool
        | SyntaxKind::ExprArray
        | SyntaxKind::ExprTuple
        | SyntaxKind::ExprGrouping
        | SyntaxKind::ExprPath
        | SyntaxKind::ExprUnary
        | SyntaxKind::ExprPostfix
        | SyntaxKind::ExprBinary
        | SyntaxKind::ExprNull
        | SyntaxKind::ExprCall
        | SyntaxKind::ExprAssignment
        | SyntaxKind::ExprIf
        | SyntaxKind::ExprWhile
        | SyntaxKind::ExprLoop
        | SyntaxKind::ExprBreak
        | SyntaxKind::ExprContinue
        | SyntaxKind::ExprReturn
    )
}

/// Check if a callable signature matches the given arity and labels
pub fn matches_signature(callable: &CallableBehavior, arity: usize, labels: &[Option<String>]) -> bool {
    let params = callable.parameters();

    // Check arity
    if params.len() != arity {
        return false;
    }

    // Check labels match
    for (param, label) in params.iter().zip(labels.iter()) {
        let param_label = param.external_label();
        let label_ref = label.as_deref();
        if param_label != label_ref {
            return false;
        }
    }

    true
}

/// Get the CallableBehavior from a symbol if it has one
pub fn get_callable_behavior(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<CallableBehavior> {
    for behavior in symbol.metadata().behaviors() {
        if behavior.kind() == KestrelBehaviorKind::Callable {
            if let Some(callable) = behavior.as_ref().downcast_ref::<CallableBehavior>() {
                return Some(callable.clone());
            }
        }
    }
    None
}


/// Create a struct type from a struct symbol.
///
/// This function downcasts the symbol to StructSymbol and creates a Ty::struct.
/// If the downcast fails (shouldn't happen for struct symbols), returns Ty::error.
pub fn create_struct_type(struct_symbol: &Arc<dyn Symbol<KestrelLanguage>>, span: Span) -> Ty {
    // Clone the Arc and use downcast_arc from downcast-rs to convert
    // Arc<dyn Symbol> to Arc<StructSymbol>
    let sym_clone = Arc::clone(struct_symbol);

    match sym_clone.downcast_arc::<StructSymbol>() {
        Ok(struct_arc) => Ty::r#struct(struct_arc, span),
        Err(_) => {
            // This shouldn't happen if we're calling this on a struct symbol
            Ty::error(span)
        }
    }
}

/// Get the container symbol from a type (for member lookup)
pub fn get_type_container(ty: &Ty, ctx: &BodyResolutionContext) -> Option<Arc<dyn Symbol<KestrelLanguage>>> {
    match ty.kind() {
        TyKind::Struct { symbol, .. } => Some(symbol.clone() as Arc<dyn Symbol<KestrelLanguage>>),
        TyKind::Protocol { symbol, .. } => Some(symbol.clone() as Arc<dyn Symbol<KestrelLanguage>>),
        TyKind::SelfType => {
            // Resolve Self to the containing struct/protocol
            // Get the function symbol, then its parent (which should be the struct/protocol)
            let function = ctx.db.symbol_by_id(ctx.function_id)?;
            let parent = function.metadata().parent()?;
            match parent.metadata().kind() {
                KestrelSymbolKind::Struct | KestrelSymbolKind::Protocol => Some(parent),
                _ => None,
            }
        }
        // TODO: Handle other types that can have members
        _ => None,
    }
}

/// Format a type for error messages
pub fn format_type(ty: &Ty) -> String {
    match ty.kind() {
        TyKind::Unit => "()".to_string(),
        TyKind::Never => "!".to_string(),
        TyKind::Bool => "Bool".to_string(),
        TyKind::String => "String".to_string(),
        TyKind::Int(bits) => format!("{:?}", bits),
        TyKind::Float(bits) => format!("{:?}", bits),
        TyKind::Tuple(elements) => {
            let items: Vec<_> = elements.iter().map(format_type).collect();
            format!("({})", items.join(", "))
        }
        TyKind::Array(elem) => format!("[{}]", format_type(elem)),
        TyKind::Function { params, return_type } => {
            let params_str: Vec<_> = params.iter().map(format_type).collect();
            format!("({}) -> {}", params_str.join(", "), format_type(return_type))
        }
        TyKind::Struct { symbol, .. } => symbol.metadata().name().value.clone(),
        TyKind::Protocol { symbol, .. } => symbol.metadata().name().value.clone(),
        TyKind::TypeParameter(param) => param.metadata().name().value.clone(),
        TyKind::TypeAlias { symbol, .. } => symbol.metadata().name().value.clone(),
        TyKind::SelfType => "Self".to_string(),
        TyKind::Inferred => "_".to_string(),
        TyKind::Error => "<error>".to_string(),
    }
}

/// Format a symbol kind for error messages
pub fn format_symbol_kind(kind: KestrelSymbolKind) -> String {
    match kind {
        KestrelSymbolKind::Field => "field".to_string(),
        KestrelSymbolKind::Function => "function".to_string(),
        KestrelSymbolKind::Import => "import".to_string(),
        KestrelSymbolKind::Initializer => "initializer".to_string(),
        KestrelSymbolKind::Module => "module".to_string(),
        KestrelSymbolKind::Protocol => "protocol".to_string(),
        KestrelSymbolKind::SourceFile => "source file".to_string(),
        KestrelSymbolKind::Struct => "struct".to_string(),
        KestrelSymbolKind::TypeAlias => "type alias".to_string(),
        KestrelSymbolKind::TypeParameter => "type parameter".to_string(),
    }
}
