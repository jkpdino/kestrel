use std::sync::Arc;

use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::{Visibility, VisibilityBehavior};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::ty::Ty;
use semantic_tree::symbol::{Symbol, SymbolTable};

// Error type for future error handling
// Uncomment when adding proper error handling
// #[derive(Debug)]
// pub struct PathResolutionError {
//     /// The segment that failed to resolve
//     pub failed_segment: String,
//     /// Index of the failed segment in the path
//     pub segment_index: usize,
//     /// Alternative symbols that were found but filtered out
//     pub alternatives: Vec<Arc<dyn Symbol<KestrelLanguage>>>,
// }

/// Internal tracking structure for resolution steps
/// Used to track information for future error reporting
#[derive(Debug)]
struct ResolutionStep {
    segment: String,
    segment_index: usize,
    candidates: Vec<Arc<dyn Symbol<KestrelLanguage>>>,
    filtered: Vec<Arc<dyn Symbol<KestrelLanguage>>>,
    selected: Option<Arc<dyn Symbol<KestrelLanguage>>>,
}

use kestrel_semantic_tree::behavior::KestrelBehaviorKind;

/// Get the TypedBehavior from a symbol if it exists
fn get_typed_behavior_ty(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> Option<Ty> {
    symbol
        .metadata()
        .behaviors()
        .into_iter()
        .find(|behavior| matches!(behavior.kind(), KestrelBehaviorKind::Typed))
        .and_then(|behavior| {
            // Use downcast to safely convert to TypedBehavior
            behavior
                .downcast_ref::<TypedBehavior>()
                .map(|typed| typed.ty().clone())
        })
}

/// Check if a symbol has TypedBehavior
fn has_typed_behavior(symbol: &Arc<dyn Symbol<KestrelLanguage>>) -> bool {
    symbol
        .metadata()
        .behaviors()
        .iter()
        .any(|behavior| behavior.as_ref().is::<TypedBehavior>())
}

/// Check if a symbol is an ancestor of the context symbol
fn is_ancestor(
    potential_ancestor: &Arc<dyn Symbol<KestrelLanguage>>,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> bool {
    let mut current = Some(context.clone());

    while let Some(symbol) = current {
        if Arc::ptr_eq(&symbol, potential_ancestor) {
            return true;
        }
        current = symbol.metadata().parent();
    }

    false
}

/// Check if a symbol is visible from the given context
///
/// Uses the visibility scope stored in VisibilityBehavior to determine if
/// the symbol is accessible from the given context.
pub fn is_visible_from(
    symbol: &Arc<dyn Symbol<KestrelLanguage>>,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> bool {
    // Find the visibility behavior
    let behaviors = symbol.metadata().behaviors();
    let visibility_behavior = behaviors
        .iter()
        .find(|behavior| behavior.as_ref().is::<VisibilityBehavior>())
        .and_then(|behavior| behavior.as_ref().downcast_ref::<VisibilityBehavior>());

    let Some(visibility_behavior) = visibility_behavior else {
        // No visibility behavior means default (internal), which is always visible
        return true;
    };

    // Check the visibility level
    match visibility_behavior.visibility() {
        Some(Visibility::Public) => {
            // Public symbols are visible everywhere
            return true;
        }
        Some(Visibility::Private) => {
            // Private symbols are only visible within the visibility scope
            let visibility_scope = visibility_behavior.visibility_scope();
            return Arc::ptr_eq(context, visibility_scope) || is_ancestor(visibility_scope, context);
        }
        Some(Visibility::Internal) => {
            // Internal symbols are visible within the same module
            // For now, treat as always visible (module-level checking requires more info)
            return true;
        }
        Some(Visibility::Fileprivate) => {
            // File-private symbols are visible within the same file
            // For now, treat as visible if in same SourceFile
            let visibility_scope = visibility_behavior.visibility_scope();
            return Arc::ptr_eq(context, visibility_scope) || is_ancestor(visibility_scope, context);
        }
        None => {
            // Default visibility (internal) - visible everywhere for now
            return true;
        }
    }
}

/// Resolve a type path like A.B.C to a Type
///
/// # Arguments
/// * `path` - The path segments to resolve (e.g., ["A", "B", "C"])
/// * `symbol_table` - The symbol table to look up the first segment
/// * `context` - The current context symbol for visibility checking
///
/// # Returns
/// * `Some(Ty)` if the path resolves to a type
/// * `None` if resolution fails
///
/// # Future Error Handling
/// When error handling is added, this will return `Result<Ty, PathResolutionError>`
/// with detailed information about:
/// - Which segment failed
/// - What alternatives were available
/// - Why they were filtered out (visibility, no TypedBehavior, etc.)
pub fn resolve_type_path(
    path: &[String],
    symbol_table: &SymbolTable<KestrelLanguage>,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Option<Ty> {
    if path.is_empty() {
        return None;
    }

    // Track resolution steps for future error handling
    let mut _steps: Vec<ResolutionStep> = Vec::new();

    // Process each segment of the path
    for (index, segment) in path.iter().enumerate() {
        // For the first segment, look it up in the symbol table
        // For subsequent segments, search through the children of the previous symbol
        let candidates: Vec<_> = if index == 0 {
            // First segment: use symbol table lookup
            let collection = symbol_table.get(segment);
            collection.multiple().to_vec()
        } else {
            // Subsequent segments: get from _steps (we'll need to track the selected symbol)
            // Get the selected symbol from the previous step
            if let Some(prev_step) = _steps.last() {
                if let Some(prev_symbol) = &prev_step.selected {
                    // Search children of the previous symbol
                    prev_symbol
                        .metadata()
                        .children()
                        .iter()
                        .filter(|symbol| symbol.metadata().name().value == *segment)
                        .cloned()
                        .collect()
                } else {
                    // Previous step had no selected symbol - fail
                    return None;
                }
            } else {
                // No previous steps but index > 0 - shouldn't happen
                return None;
            }
        };

        // Filter by visibility and TypedBehavior
        let filtered: Vec<_> = candidates
            .iter()
            .filter(|symbol| {
                // Must be visible from context
                is_visible_from(symbol, context) &&
                // Must have TypedBehavior
                has_typed_behavior(symbol)
            })
            .cloned()
            .collect();

        // Track this step for future error handling
        let selected = filtered.first().cloned();
        _steps.push(ResolutionStep {
            segment: segment.clone(),
            segment_index: index,
            candidates,
            filtered: filtered.clone(),
            selected: selected.clone(),
        });

        // If no matches, resolution failed
        if filtered.is_empty() {
            // TODO: When error handling is added, construct PathResolutionError here
            // let error = PathResolutionError {
            //     failed_segment: segment.clone(),
            //     segment_index: index,
            //     alternatives: candidates,
            // };
            // return Err(error);
            return None;
        }

        // If this is the last segment, we're done
        if index == path.len() - 1 {
            // Extract the type from the TypedBehavior
            if let Some(symbol) = filtered.first() {
                return get_typed_behavior_ty(symbol);
            }
            return None;
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_semantic_tree::symbol::class::ClassSymbol;
    use kestrel_span::Name;

    #[test]
    fn test_simple_path_resolution() {
        // Create a root symbol and symbol table
        let root = create_test_root();
        let mut symbol_table = SymbolTable::new();

        let class_name = Name::new("MyClass".to_string(), 0..7);
        let visibility = VisibilityBehavior::new(Some(Visibility::Public), 0..6, root.clone());

        let class_symbol = ClassSymbol::new(class_name, 0..20, visibility);
        let class_arc = Arc::new(class_symbol);

        // Add TypedBehavior after creation (following the pattern in resolvers/class.rs)
        let class_type = Ty::path(vec!["MyClass".to_string()], 0..7);
        let typed_behavior = TypedBehavior::new(class_type, 0..7);
        class_arc.metadata().add_behavior(typed_behavior);

        let class_arc_dyn: Arc<dyn Symbol<KestrelLanguage>> = class_arc;

        // Add to symbol table
        symbol_table.insert(class_arc_dyn.clone());
        root.metadata().add_child(&class_arc_dyn);

        // Resolve the path "MyClass"
        let path = vec!["MyClass".to_string()];
        let result = resolve_type_path(&path, &symbol_table, &root);

        assert!(result.is_some());
        let ty = result.unwrap();
        assert!(ty.is_path());

        if let Some(segments) = ty.as_path() {
            assert_eq!(segments.len(), 1);
            assert_eq!(segments[0], "MyClass");
        }
    }

    #[test]
    fn test_path_not_found() {
        let root = create_test_root();
        let symbol_table = SymbolTable::new();

        // Try to resolve a path that doesn't exist
        let path = vec!["NonExistent".to_string()];
        let result = resolve_type_path(&path, &symbol_table, &root);

        assert!(result.is_none());
    }

    #[test]
    fn test_nested_class_resolution() {
        // Create a root symbol and symbol table
        let root = create_test_root();
        let mut symbol_table = SymbolTable::new();

        // Create outer class
        let outer_name = Name::new("Outer".to_string(), 0..5);
        let outer_visibility =
            VisibilityBehavior::new(Some(Visibility::Public), 0..6, root.clone());
        let outer_symbol = ClassSymbol::new(outer_name, 0..50, outer_visibility);
        let outer_arc = Arc::new(outer_symbol);

        // Add TypedBehavior to outer class
        let outer_type = Ty::path(vec!["Outer".to_string()], 0..5);
        let outer_typed = TypedBehavior::new(outer_type, 0..5);
        outer_arc.metadata().add_behavior(outer_typed);

        let outer_arc_dyn: Arc<dyn Symbol<KestrelLanguage>> = outer_arc.clone();

        // Create inner class (child of outer)
        let inner_name = Name::new("Inner".to_string(), 10..15);
        let inner_visibility =
            VisibilityBehavior::new(Some(Visibility::Public), 10..16, root.clone());
        let inner_symbol = ClassSymbol::new(inner_name, 10..40, inner_visibility);
        let inner_arc = Arc::new(inner_symbol);

        // Add TypedBehavior to inner class
        let inner_type = Ty::path(vec!["Outer".to_string(), "Inner".to_string()], 10..15);
        let inner_typed = TypedBehavior::new(inner_type, 10..15);
        inner_arc.metadata().add_behavior(inner_typed);

        let inner_arc_dyn: Arc<dyn Symbol<KestrelLanguage>> = inner_arc;

        // Make inner a child of outer
        outer_arc_dyn.metadata().add_child(&inner_arc_dyn);

        // Add outer to symbol table (inner is only accessible through outer)
        symbol_table.insert(outer_arc_dyn.clone());
        root.metadata().add_child(&outer_arc_dyn);

        // Resolve the path "Outer.Inner"
        let path = vec!["Outer".to_string(), "Inner".to_string()];
        let result = resolve_type_path(&path, &symbol_table, &root);

        assert!(
            result.is_some(),
            "Failed to resolve nested path Outer.Inner"
        );
        let ty = result.unwrap();
        assert!(ty.is_path(), "Result should be a path type");

        if let Some(segments) = ty.as_path() {
            assert_eq!(segments.len(), 2);
            assert_eq!(segments[0], "Outer");
            assert_eq!(segments[1], "Inner");
        }
    }

    #[test]
    fn test_nested_class_private_inner() {
        // Test that private inner classes are not accessible from outside
        let root = create_test_root();
        let mut symbol_table = SymbolTable::new();

        // Create outer class
        let outer_name = Name::new("Outer".to_string(), 0..5);
        let outer_visibility =
            VisibilityBehavior::new(Some(Visibility::Public), 0..6, root.clone());
        let outer_symbol = ClassSymbol::new(outer_name, 0..50, outer_visibility);
        let outer_arc = Arc::new(outer_symbol);

        // Add TypedBehavior to outer class
        let outer_type = Ty::path(vec!["Outer".to_string()], 0..5);
        let outer_typed = TypedBehavior::new(outer_type, 0..5);
        outer_arc.metadata().add_behavior(outer_typed);

        let outer_arc_dyn: Arc<dyn Symbol<KestrelLanguage>> = outer_arc.clone();

        // Create PRIVATE inner class
        let inner_name = Name::new("Inner".to_string(), 10..15);
        let inner_visibility =
            VisibilityBehavior::new(Some(Visibility::Private), 10..17, outer_arc_dyn.clone());
        let inner_symbol = ClassSymbol::new(inner_name, 10..40, inner_visibility);
        let inner_arc = Arc::new(inner_symbol);

        // Add TypedBehavior to inner class
        let inner_type = Ty::path(vec!["Outer".to_string(), "Inner".to_string()], 10..15);
        let inner_typed = TypedBehavior::new(inner_type, 10..15);
        inner_arc.metadata().add_behavior(inner_typed);

        let inner_arc_dyn: Arc<dyn Symbol<KestrelLanguage>> = inner_arc;

        // Make inner a child of outer
        outer_arc_dyn.metadata().add_child(&inner_arc_dyn);

        // Add outer to symbol table
        symbol_table.insert(outer_arc_dyn.clone());
        root.metadata().add_child(&outer_arc_dyn);

        // Try to resolve "Outer.Inner" from root context - should fail because Inner is private
        let path = vec!["Outer".to_string(), "Inner".to_string()];
        let result = resolve_type_path(&path, &symbol_table, &root);

        assert!(
            result.is_none(),
            "Private inner class should not be accessible from root context"
        );
    }

    #[test]
    fn test_deeply_nested_classes() {
        // Test A.B.C resolution
        let root = create_test_root();
        let mut symbol_table = SymbolTable::new();

        // Create class A
        let a_name = Name::new("A".to_string(), 0..1);
        let a_visibility = VisibilityBehavior::new(Some(Visibility::Public), 0..6, root.clone());
        let a_symbol = ClassSymbol::new(a_name, 0..100, a_visibility);
        let a_arc = Arc::new(a_symbol);
        let a_type = Ty::path(vec!["A".to_string()], 0..1);
        let a_typed = TypedBehavior::new(a_type, 0..1);
        a_arc.metadata().add_behavior(a_typed);
        let a_arc_dyn: Arc<dyn Symbol<KestrelLanguage>> = a_arc;

        // Create class B (child of A)
        let b_name = Name::new("B".to_string(), 10..11);
        let b_visibility = VisibilityBehavior::new(Some(Visibility::Public), 10..16, root.clone());
        let b_symbol = ClassSymbol::new(b_name, 10..80, b_visibility);
        let b_arc = Arc::new(b_symbol);
        let b_type = Ty::path(vec!["A".to_string(), "B".to_string()], 10..11);
        let b_typed = TypedBehavior::new(b_type, 10..11);
        b_arc.metadata().add_behavior(b_typed);
        let b_arc_dyn: Arc<dyn Symbol<KestrelLanguage>> = b_arc;

        // Create class C (child of B)
        let c_name = Name::new("C".to_string(), 20..21);
        let c_visibility = VisibilityBehavior::new(Some(Visibility::Public), 20..26, root.clone());
        let c_symbol = ClassSymbol::new(c_name, 20..50, c_visibility);
        let c_arc = Arc::new(c_symbol);
        let c_type = Ty::path(
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            20..21,
        );
        let c_typed = TypedBehavior::new(c_type, 20..21);
        c_arc.metadata().add_behavior(c_typed);
        let c_arc_dyn: Arc<dyn Symbol<KestrelLanguage>> = c_arc;

        // Build the hierarchy
        b_arc_dyn.metadata().add_child(&c_arc_dyn);
        a_arc_dyn.metadata().add_child(&b_arc_dyn);

        // Add A to symbol table
        symbol_table.insert(a_arc_dyn.clone());
        root.metadata().add_child(&a_arc_dyn);

        // Resolve A.B.C
        let path = vec!["A".to_string(), "B".to_string(), "C".to_string()];
        let result = resolve_type_path(&path, &symbol_table, &root);

        assert!(
            result.is_some(),
            "Failed to resolve deeply nested path A.B.C"
        );
        let ty = result.unwrap();
        assert!(ty.is_path());

        if let Some(segments) = ty.as_path() {
            assert_eq!(segments.len(), 3);
            assert_eq!(segments[0], "A");
            assert_eq!(segments[1], "B");
            assert_eq!(segments[2], "C");
        }
    }

    // Helper to create a test root symbol
    fn create_test_root() -> Arc<dyn Symbol<KestrelLanguage>> {
        use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;
        use semantic_tree::symbol::SymbolMetadataBuilder;

        let root_name = Name::new("Root".to_string(), 0..4);
        // Use Class as the symbol kind since it's the only one available
        let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::Class)
            .with_name(root_name)
            .with_declaration_span(0..4)
            .with_span(0..100)
            .build();

        #[derive(Debug)]
        struct TestRootSymbol {
            metadata: semantic_tree::symbol::SymbolMetadata<KestrelLanguage>,
        }

        impl Symbol<KestrelLanguage> for TestRootSymbol {
            fn metadata(&self) -> &semantic_tree::symbol::SymbolMetadata<KestrelLanguage> {
                &self.metadata
            }
        }

        Arc::new(TestRootSymbol { metadata })
    }
}
