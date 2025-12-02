//! Validation pass for protocol conformance and inheritance
//!
//! This pass checks:
//! - Circular protocol inheritance (protocol A: B where protocol B: A)
//! - Conforming types implement all required methods
//! - Method signatures match protocol requirements

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use kestrel_reporting::DiagnosticContext;
use kestrel_semantic_tree::behavior::callable::{CallableSignature, SignatureType};
use kestrel_semantic_tree::behavior::conformances::ConformancesBehavior;
use kestrel_semantic_tree::behavior::KestrelBehaviorKind;
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::function::FunctionSymbol;
use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
use kestrel_semantic_tree::symbol::protocol::ProtocolSymbol;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::ty::{Ty, TyKind};
use semantic_tree::symbol::{Symbol, SymbolId};

use crate::db::SemanticDatabase;
use crate::queries::{Db, TypePathResolution};
use crate::validation::{ValidationConfig, ValidationPass};

/// Validation pass that checks protocol conformance and inheritance rules
pub struct ConformancePass;

impl ConformancePass {
    const NAME: &'static str = "conformance";
}

/// Get the resolved conformances from a symbol's ConformancesBehavior
///
/// Returns the last ConformancesBehavior's conformances (the resolved ones from bind phase).
/// Returns an empty slice if no ConformancesBehavior exists.
fn get_conformances(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Vec<Ty> {
    symbol
        .metadata()
        .behaviors()
        .into_iter()
        .rev()
        .find_map(|b| {
            if b.kind() == KestrelBehaviorKind::Conformances {
                b.as_ref().downcast_ref::<ConformancesBehavior>().map(|c| c.conformances().to_vec())
            } else {
                None
            }
        })
        .unwrap_or_default()
}

/// Check if a symbol has any conformances
fn has_conformances(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> bool {
    !get_conformances(symbol).is_empty()
}

/// Get the Arc<ProtocolSymbol> from a symbol's TypedBehavior
///
/// Protocol symbols have a TypedBehavior that stores their Ty::Protocol,
/// which contains the Arc<ProtocolSymbol>.
fn get_protocol_arc_from_symbol(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<Arc<ProtocolSymbol>> {
    use kestrel_semantic_tree::behavior::typed::TypedBehavior;

    symbol
        .metadata()
        .behaviors()
        .into_iter()
        .find_map(|b| {
            if b.kind() == KestrelBehaviorKind::Typed {
                if let Some(tb) = b.as_ref().downcast_ref::<TypedBehavior>() {
                    if let TyKind::Protocol { symbol, .. } = tb.ty().kind() {
                        return Some(symbol.clone());
                    }
                }
            }
            None
        })
}

impl ValidationPass for ConformancePass {
    fn name(&self) -> &'static str {
        Self::NAME
    }

    fn validate(
        &self,
        root: &Arc<dyn Symbol<KestrelLanguage>>,
        db: &SemanticDatabase,
        diagnostics: &mut DiagnosticContext,
        config: &ValidationConfig,
    ) {
        validate_symbol(root, db, diagnostics, config);
    }
}

/// Recursively validate symbols, looking for protocols and structs with conformances
fn validate_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let kind = symbol.metadata().kind();

    // Check protocols for circular inheritance
    if kind == KestrelSymbolKind::Protocol {
        // We need Arc<ProtocolSymbol> to pass to has_inheritance_cycle
        // Try to get it via the typed behavior which stores the protocol type
        let protocol_arc = get_protocol_arc_from_symbol(symbol);
        if let Some(protocol) = protocol_arc {
            check_circular_inheritance(&protocol, symbol, diagnostics, config);
        }
    }

    // Check structs for protocol conformance
    if kind == KestrelSymbolKind::Struct {
        if let Some(struct_sym) = symbol.as_ref().downcast_ref::<StructSymbol>() {
            check_struct_conformance(struct_sym, symbol, db, diagnostics, config);
        }
    }

    // Recursively check children
    for child in symbol.metadata().children() {
        validate_symbol(&child, db, diagnostics, config);
    }
}

/// Check if a protocol has circular inheritance
fn check_circular_inheritance(
    protocol: &Arc<ProtocolSymbol>,
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    let protocol_name = &protocol.metadata().name().value;

    // Track visited protocols to detect cycles
    let mut visited = HashSet::new();
    let mut path = Vec::new();

    if has_inheritance_cycle(protocol, &mut visited, &mut path) {
        let span = protocol.metadata().declaration_span().clone();
        let file_id = get_file_id_for_symbol(symbol, diagnostics);

        let message = if config.debug_mode {
            format!(
                "[{}] protocol '{}' has circular inheritance",
                ConformancePass::NAME,
                protocol_name
            )
        } else {
            format!("protocol '{}' has circular inheritance", protocol_name)
        };

        let diagnostic = kestrel_reporting::Diagnostic::error()
            .with_message(message)
            .with_labels(vec![
                kestrel_reporting::Label::primary(file_id, span)
                    .with_message("circular inheritance detected")
            ]);

        diagnostics.add_diagnostic(diagnostic);
    }
}

/// Recursively check for inheritance cycles
fn has_inheritance_cycle(
    protocol: &Arc<ProtocolSymbol>,
    visited: &mut HashSet<SymbolId>,
    path: &mut Vec<SymbolId>,
) -> bool {
    let id = protocol.metadata().id();

    // Check if we've encountered this protocol in the current path (cycle)
    if path.contains(&id) {
        return true;
    }

    // Check if we've already fully visited this protocol (no cycle from here)
    if visited.contains(&id) {
        return false;
    }

    path.push(id);

    // Check all inherited protocols (via ConformancesBehavior)
    let protocol_dyn = protocol.clone() as Arc<dyn Symbol<KestrelLanguage>>;
    for inherited_ty in get_conformances(&protocol_dyn) {
        if let TyKind::Protocol { symbol, .. } = inherited_ty.kind() {
            if has_inheritance_cycle(symbol, visited, path) {
                return true;
            }
        }
        // Note: Error types are unresolved and can't be checked for cycles yet
        // This is handled during type resolution
    }

    path.pop();
    visited.insert(id);
    false
}

/// Get the file_id for a symbol by walking up to its SourceFile parent
fn get_file_id_for_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    diagnostics: &DiagnosticContext,
) -> usize {
    let mut current = symbol.clone();
    loop {
        if current.metadata().kind() == KestrelSymbolKind::SourceFile {
            let file_name = current.metadata().name().value.clone();
            return diagnostics.get_file_id(&file_name).unwrap_or(0);
        }
        match current.metadata().parent() {
            Some(parent) => current = parent,
            None => return 0,
        }
    }
}

/// Check that a struct implements all required methods from its conformances
fn check_struct_conformance(
    struct_sym: &StructSymbol,
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    db: &SemanticDatabase,
    diagnostics: &mut DiagnosticContext,
    config: &ValidationConfig,
) {
    // Get conformances from ConformancesBehavior
    let conformances = get_conformances(symbol);

    // Skip if no conformances
    if conformances.is_empty() {
        return;
    }

    let struct_name = &struct_sym.metadata().name().value;
    let struct_id = struct_sym.metadata().id();
    let file_id = get_file_id_for_symbol(symbol, diagnostics);

    // Collect all methods implemented by the struct (signature -> (method, return_type))
    let struct_methods = collect_methods_from_symbol(symbol);
    let struct_method_map: HashMap<CallableSignature, (&Arc<FunctionSymbol>, SignatureType)> = struct_methods
        .iter()
        .map(|f| (f.signature(), (f, SignatureType::from_ty(&f.return_type()))))
        .collect();

    // Check each conformance
    for conformance_ty in &conformances {
        // Resolve the protocol type
        let protocol_symbol = match resolve_protocol_type(conformance_ty, struct_id, db) {
            Some(proto) => proto,
            None => continue, // Unresolved types are handled by type resolution errors
        };

        let protocol_name = &protocol_symbol.metadata().name().value;

        // Collect all required methods from the protocol (including inherited)
        let required_methods = collect_all_protocol_methods(&protocol_symbol, db);

        // Check each required method
        for (sig, method) in &required_methods {
            let method_name = &method.metadata().name().value;
            let required_return_type = SignatureType::from_ty(&method.return_type());

            match struct_method_map.get(sig) {
                None => {
                    // Method not found - report error
                    let span = struct_sym.metadata().declaration_span().clone();

                    let message = if config.debug_mode {
                        format!(
                            "[{}] struct '{}' does not implement required method '{}' from protocol '{}'",
                            ConformancePass::NAME,
                            struct_name,
                            method_name,
                            protocol_name
                        )
                    } else {
                        format!(
                            "struct '{}' does not implement required method '{}' from protocol '{}'",
                            struct_name,
                            method_name,
                            protocol_name
                        )
                    };

                    let diagnostic = kestrel_reporting::Diagnostic::error()
                        .with_message(message)
                        .with_labels(vec![
                            kestrel_reporting::Label::primary(file_id, span)
                                .with_message(format!("missing method '{}'", method_name))
                        ]);

                    diagnostics.add_diagnostic(diagnostic);
                }
                Some((_struct_method, struct_return_type)) => {
                    // Method found - check return type matches
                    if struct_return_type != &required_return_type {
                        let span = struct_sym.metadata().declaration_span().clone();

                        let message = if config.debug_mode {
                            format!(
                                "[{}] struct '{}' does not implement required method '{}' from protocol '{}' (return type mismatch)",
                                ConformancePass::NAME,
                                struct_name,
                                method_name,
                                protocol_name
                            )
                        } else {
                            format!(
                                "struct '{}' does not implement required method '{}' from protocol '{}'",
                                struct_name,
                                method_name,
                                protocol_name
                            )
                        };

                        let diagnostic = kestrel_reporting::Diagnostic::error()
                            .with_message(message)
                            .with_labels(vec![
                                kestrel_reporting::Label::primary(file_id, span)
                                    .with_message(format!("method '{}' has wrong return type", method_name))
                            ]);

                        diagnostics.add_diagnostic(diagnostic);
                    }
                }
            }
        }
    }
}

/// Resolve a Ty to a ProtocolSymbol if it's a protocol type
fn resolve_protocol_type(
    ty: &kestrel_semantic_tree::ty::Ty,
    context: SymbolId,
    db: &SemanticDatabase,
) -> Option<Arc<ProtocolSymbol>> {
    match ty.kind() {
        // Already resolved to a protocol
        TyKind::Protocol { symbol, .. } => Some(symbol.clone()),

        // Error types and other unresolved types - resolution errors are reported elsewhere
        _ => None,
    }
}

/// Collect all methods from a symbol (struct or protocol)
fn collect_methods_from_symbol(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Vec<Arc<FunctionSymbol>> {
    symbol
        .metadata()
        .children()
        .into_iter()
        .filter(|child| child.metadata().kind() == KestrelSymbolKind::Function)
        .filter_map(|child| {
            // Try to downcast to FunctionSymbol
            child.into_any_arc().downcast::<FunctionSymbol>().ok()
        })
        .collect()
}

/// Collect all required methods from a protocol, including inherited protocols
fn collect_all_protocol_methods(
    protocol: &Arc<ProtocolSymbol>,
    db: &SemanticDatabase,
) -> HashMap<CallableSignature, Arc<FunctionSymbol>> {
    let mut methods = HashMap::new();
    let mut visited = HashSet::new();

    collect_protocol_methods_recursive(protocol, db, &mut methods, &mut visited);

    methods
}

/// Recursively collect methods from a protocol and its inherited protocols
fn collect_protocol_methods_recursive(
    protocol: &Arc<ProtocolSymbol>,
    db: &SemanticDatabase,
    methods: &mut HashMap<CallableSignature, Arc<FunctionSymbol>>,
    visited: &mut HashSet<SymbolId>,
) {
    let id = protocol.metadata().id();

    // Avoid visiting the same protocol twice (handles diamond inheritance)
    if visited.contains(&id) {
        return;
    }
    visited.insert(id);

    // First, collect methods from inherited protocols (so child methods can override)
    let protocol_dyn = protocol.clone() as Arc<dyn Symbol<KestrelLanguage>>;
    for inherited_ty in get_conformances(&protocol_dyn) {
        if let Some(inherited_protocol) = resolve_protocol_type(&inherited_ty, id, db) {
            collect_protocol_methods_recursive(&inherited_protocol, db, methods, visited);
        }
    }

    // Then collect methods from this protocol (overrides inherited)
    let protocol_dyn = protocol.clone() as Arc<dyn Symbol<KestrelLanguage>>;
    for method in collect_methods_from_symbol(&protocol_dyn) {
        let sig = method.signature();
        methods.insert(sig, method);
    }
}
