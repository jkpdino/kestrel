//! Validation pass for detecting circular type alias dependencies
//!
//! This pass detects cycles in type aliases such as:
//! - Direct self-references: `type A = A;`
//! - Two-way cycles: `type A = B; type B = A;`
//! - Longer chains: `type A = B; type B = C; type C = A;`
//!
//! The algorithm uses a DFS-based approach to follow type alias chains
//! and detect cycles using a CycleDetector.

use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::error::{CircularTypeAliasError, CycleParticipant};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::type_alias::TypeAliasTypedBehavior;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use semantic_tree::cycle::CycleDetector;
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::db::SemanticDatabase;
use crate::utils::get_file_id_for_symbol;
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass that detects circular type alias dependencies
pub struct TypeAliasCyclePass;

impl TypeAliasCyclePass {
    const NAME: &'static str = "type_alias_cycles";
}

impl ValidationPass for TypeAliasCyclePass {
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

/// Recursively walk the symbol tree and check for cycles in type aliases
fn check_cycles_in_tree(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    let kind = symbol.metadata().kind();

    // If this is a type alias, check for cycles starting from it
    if kind == KestrelSymbolKind::TypeAlias {
        check_type_alias_for_cycles(symbol, db, diagnostics);
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        check_cycles_in_tree(&child, db, diagnostics);
    }
}

/// Check if a specific type alias participates in a cycle
fn check_type_alias_for_cycles(
    type_alias: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
) {
    // Get the file_id for error reporting
    let file_id = get_file_id_for_symbol(type_alias, diagnostics);

    // Get the TypeAliasTypedBehavior which contains the resolved aliased type
    let behaviors = type_alias.metadata().behaviors();
    let type_alias_typed = behaviors.iter().find_map(|b| {
        if matches!(b.kind(), KestrelBehaviorKind::TypeAliasTyped) {
            b.as_ref().downcast_ref::<TypeAliasTypedBehavior>()
        } else {
            None
        }
    });

    // If there's no resolved type, skip (binding failed)
    let Some(resolved) = type_alias_typed else {
        return;
    };

    // Create a cycle detector and start tracking this type alias
    let mut visited: CycleDetector<SymbolId> = CycleDetector::new();
    let symbol_id = type_alias.metadata().id();

    // Enter this type alias into the detector
    if visited.enter(symbol_id).is_ok() {
        // Follow the type alias chain
        if let Some(cycle) = follow_type_alias_chain(resolved.resolved_ty(), &mut visited) {
            // Build the error with cycle participants
            let origin = CycleParticipant {
                name: type_alias.metadata().name().value.clone(),
                name_span: type_alias.metadata().name().span.clone(),
                file_id: Some(file_id),
            };

            let cycle_participants: Vec<CycleParticipant> = cycle
                .cycle()
                .iter()
                .skip(1) // Skip the origin (which is the first element)
                .filter_map(|&id| {
                    db.symbol_by_id(id).map(|s| CycleParticipant {
                        name: s.metadata().name().value.clone(),
                        name_span: s.metadata().name().span.clone(),
                        file_id: Some(get_file_id_for_symbol(&s, diagnostics)),
                    })
                })
                .collect();

            diagnostics.throw(
                CircularTypeAliasError {
                    origin,
                    cycle: cycle_participants,
                },
                file_id,
            );
        }
    }
}

/// Follow a type alias chain and detect cycles
///
/// This function recursively traverses type structures looking for TypeAlias types.
/// When it finds one, it checks if we've already visited it (cycle detection).
///
/// Returns Some(cycle) if a cycle is detected, None otherwise.
fn follow_type_alias_chain(
    ty: &Ty,
    visited: &mut CycleDetector<SymbolId>,
) -> Option<semantic_tree::cycle::Cycle<SymbolId>> {
    match ty.kind() {
        TyKind::TypeAlias {
            symbol: alias_symbol,
            ..
        } => {
            let alias_id = alias_symbol.metadata().id();

            // Try to enter - if it fails, we found a cycle
            if let Err(cycle) = visited.enter(alias_id) {
                return Some(cycle);
            }

            // Get the resolved type from this alias
            let behaviors = alias_symbol.metadata().behaviors();
            let type_alias_typed = behaviors.iter().find_map(|b| {
                if matches!(b.kind(), KestrelBehaviorKind::TypeAliasTyped) {
                    b.as_ref().downcast_ref::<TypeAliasTypedBehavior>()
                } else {
                    None
                }
            });

            if let Some(resolved) = type_alias_typed {
                // Recursively follow the chain
                let result = follow_type_alias_chain(resolved.resolved_ty(), visited);
                visited.exit();
                return result;
            }

            visited.exit();
            None
        }
        TyKind::Tuple(elements) => {
            // Check each element of the tuple
            for elem in elements {
                if let Some(cycle) = follow_type_alias_chain(elem, visited) {
                    return Some(cycle);
                }
            }
            None
        }
        TyKind::Function {
            params,
            return_type,
        } => {
            // Check parameter types
            for param in params {
                if let Some(cycle) = follow_type_alias_chain(param, visited) {
                    return Some(cycle);
                }
            }
            // Check return type
            follow_type_alias_chain(return_type, visited)
        }
        // For other types, no cycle possible
        _ => None,
    }
}

