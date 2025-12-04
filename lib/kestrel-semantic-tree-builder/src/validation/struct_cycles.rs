//! Validation pass for detecting circular struct containment
//!
//! This pass detects cycles in struct field types that would create infinite-size types:
//! - Direct self-references: `struct Node { let next: Node }`
//! - Two-way cycles: `struct A { let b: B } struct B { let a: A }`
//! - Longer chains: `struct A { let b: B } struct B { let c: C } struct C { let a: A }`
//!
//! The algorithm uses DFS to follow struct field types and detect cycles.

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use semantic_tree::cycle::CycleDetector;
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::db::SemanticDatabase;
use crate::diagnostics::{CircularStructContainmentError, CycleMember, SelfContainingStructError};
use crate::utils::get_file_id_for_symbol;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass that detects circular struct containment
pub struct StructCyclePass;

impl StructCyclePass {
    const NAME: &'static str = "struct_cycles";
}

impl ValidationPass for StructCyclePass {
    fn name(&self) -> &'static str {
        Self::NAME
    }

    fn validate(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        _config: &ValidationConfig,
    ) {
        check_cycles_in_tree(root, db, diagnostics);
    }
}

/// Recursively walk the symbol tree and check for cycles in struct containment
fn check_cycles_in_tree(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    let kind = symbol.metadata().kind();

    // If this is a struct, check for containment cycles starting from it
    if kind == KestrelSymbolKind::Struct {
        if let Some(struct_sym) = symbol.as_ref().downcast_ref::<StructSymbol>() {
            check_struct_for_cycles(struct_sym, symbol, db, diagnostics);
        }
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        check_cycles_in_tree(&child, db, diagnostics);
    }
}

/// Check if a specific struct participates in a containment cycle
fn check_struct_for_cycles(
    struct_sym: &StructSymbol,
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    let file_id = get_file_id_for_symbol(symbol, diagnostics);
    let struct_id = struct_sym.metadata().id();
    let struct_name = struct_sym.metadata().name().value.clone();

    // Get all fields and check their types
    for field in symbol.metadata().children() {
        if field.metadata().kind() != KestrelSymbolKind::Field {
            continue;
        }

        // Get the field's type from TypedBehavior
        let field_ty = field.metadata().behaviors().iter().find_map(|b| {
            if matches!(b.kind(), KestrelBehaviorKind::Typed) {
                b.as_ref().downcast_ref::<TypedBehavior>().map(|tb| tb.ty().clone())
            } else {
                None
            }
        });

        let Some(field_ty) = field_ty else {
            continue;
        };

        let field_name = field.metadata().name().value.clone();
        let field_span = field.metadata().span().clone();

        // Check if this field's type creates a cycle
        let mut detector: CycleDetector<SymbolId> = CycleDetector::new();

        // Enter the current struct
        if detector.enter(struct_id).is_err() {
            // Shouldn't happen on first entry
            continue;
        }

        if let Some(cycle) = check_type_for_struct_cycle(&field_ty, &mut detector, db) {
            // Check if it's a self-cycle (direct self-reference)
            if cycle.is_self_cycle() {
                diagnostics.throw(
                    SelfContainingStructError {
                        struct_name: struct_name.clone(),
                        struct_span: struct_sym.metadata().declaration_span().clone(),
                        field_name,
                        field_span,
                    },
                    file_id,
                );
            } else {
                // Multi-struct cycle
                let origin = CycleMember {
                    name: struct_name.clone(),
                    span: struct_sym.metadata().declaration_span().clone(),
                    file_id: Some(file_id),
                };

                let cycle_members: Vec<CycleMember> = cycle
                    .cycle()
                    .iter()
                    .skip(1) // Skip the origin
                    .filter_map(|&id| {
                        db.symbol_by_id(id).map(|s| CycleMember {
                            name: s.metadata().name().value.clone(),
                            span: s.metadata().declaration_span().clone(),
                            file_id: Some(get_file_id_for_symbol(&s, diagnostics)),
                        })
                    })
                    .collect();

                diagnostics.throw(
                    CircularStructContainmentError {
                        origin,
                        cycle: cycle_members,
                        field_name,
                        field_span,
                    },
                    file_id,
                );
            }
        }
    }
}

/// Recursively check a type for struct cycles
///
/// Returns Some(cycle) if a cycle is detected, None otherwise.
fn check_type_for_struct_cycle(
    ty: &Ty,
    detector: &mut CycleDetector<SymbolId>,
    db: &SemanticDatabase,
) -> Option<semantic_tree::cycle::Cycle<SymbolId>> {
    match ty.kind() {
        TyKind::Struct { symbol, .. } => {
            let struct_id = symbol.metadata().id();

            // Try to enter - if it fails, we found a cycle
            if let Err(cycle) = detector.enter(struct_id) {
                return Some(cycle);
            }

            // Check all fields of this struct for cycles
            let struct_dyn = symbol.clone() as Arc<dyn Symbol<KestrelLanguage>>;
            for field in struct_dyn.metadata().children() {
                if field.metadata().kind() != KestrelSymbolKind::Field {
                    continue;
                }

                // Get the field's type
                let field_ty = field.metadata().behaviors().iter().find_map(|b| {
                    if matches!(b.kind(), KestrelBehaviorKind::Typed) {
                        b.as_ref().downcast_ref::<TypedBehavior>().map(|tb| tb.ty().clone())
                    } else {
                        None
                    }
                });

                if let Some(field_ty) = field_ty {
                    if let Some(cycle) = check_type_for_struct_cycle(&field_ty, detector, db) {
                        detector.exit();
                        return Some(cycle);
                    }
                }
            }

            detector.exit();
            None
        }
        TyKind::Tuple(elements) => {
            // Check each element of the tuple
            for elem in elements {
                if let Some(cycle) = check_type_for_struct_cycle(elem, detector, db) {
                    return Some(cycle);
                }
            }
            None
        }
        // Arrays, optionals, and other indirection types break the cycle
        // (they can hold a reference/pointer rather than embedding the value)
        TyKind::Array(_) => None,
        // Function types don't directly embed struct values
        TyKind::Function { .. } => None,
        // Primitives and other types don't create struct cycles
        _ => None,
    }
}

