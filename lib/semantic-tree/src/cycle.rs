//! Cycle detection primitives for graph traversal operations.
//!
//! This module provides reusable utilities for detecting cycles in various
//! scenarios such as:
//! - Type alias resolution (detecting circular type aliases)
//! - Import resolution (detecting circular imports)
//! - Symbol dependency analysis
//!
//! # Example
//!
//! ```ignore
//! use semantic_tree::cycle::CycleDetector;
//!
//! let mut detector = CycleDetector::new();
//!
//! // Enter a node - returns Err if cycle detected
//! if let Err(cycle) = detector.enter("A") {
//!     // Handle cycle: cycle.participants() returns ["A"]
//! }
//!
//! // Nested entry
//! detector.enter("B").unwrap();
//!
//! // This would detect a cycle back to "A"
//! if let Err(cycle) = detector.enter("A") {
//!     // cycle.participants() returns ["A", "B", "A"]
//! }
//!
//! // Exit when done with a node
//! detector.exit();
//! ```

use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

/// A detected cycle in a graph traversal.
///
/// Contains information about the cycle including all participants
/// in the order they were visited.
#[derive(Debug, Clone)]
pub struct Cycle<T> {
    /// The path from the start of traversal to the cycle point.
    /// The last element is the same as some earlier element, indicating the cycle.
    path: Vec<T>,
    /// The index in `path` where the cycle begins (the repeated element).
    cycle_start: usize,
}

impl<T: Clone + Debug> Cycle<T> {
    /// Get the full path that led to discovering the cycle.
    ///
    /// The last element will be the same as the element at `cycle_start_index()`.
    pub fn path(&self) -> &[T] {
        &self.path
    }

    /// Get just the cycle portion of the path.
    ///
    /// This is the sequence from the first occurrence of the repeated element
    /// to (but not including) its second occurrence.
    pub fn cycle(&self) -> &[T] {
        &self.path[self.cycle_start..self.path.len() - 1]
    }

    /// Get the element that caused the cycle (appeared twice).
    pub fn cycle_cause(&self) -> &T {
        self.path.last().expect("cycle path should not be empty")
    }

    /// Get the index in the path where the cycle begins.
    pub fn cycle_start_index(&self) -> usize {
        self.cycle_start
    }

    /// Returns true if this is a self-cycle (element references itself directly).
    pub fn is_self_cycle(&self) -> bool {
        self.path.len() == 2 && self.cycle_start == 0
    }
}


/// A cycle detector for tracking visited nodes during graph traversal.
///
/// This is useful for detecting cycles in recursive resolution operations
/// like type alias resolution, import resolution, etc.
///
/// # Type Parameters
///
/// * `T` - The type used to identify nodes. Must be `Clone + Eq + Hash`.
///   Common choices include `SymbolId`, `String`, or custom identifier types.
#[derive(Debug)]
pub struct CycleDetector<T> {
    /// Set of currently active (in-progress) nodes for O(1) cycle detection.
    active: HashSet<T>,
    /// Stack of nodes in visitation order for cycle path reconstruction.
    stack: Vec<T>,
}

impl<T: Clone + Eq + Hash + Debug> CycleDetector<T> {
    /// Create a new empty cycle detector.
    pub fn new() -> Self {
        Self {
            active: HashSet::new(),
            stack: Vec::new(),
        }
    }

    /// Create a new cycle detector with expected capacity.
    ///
    /// Use this when you have a reasonable estimate of the maximum
    /// depth of recursion to avoid reallocations.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            active: HashSet::with_capacity(capacity),
            stack: Vec::with_capacity(capacity),
        }
    }

    /// Enter a node in the traversal.
    ///
    /// Returns `Ok(())` if the node hasn't been visited yet in the current path,
    /// or `Err(Cycle)` if entering this node would create a cycle.
    ///
    /// If `Ok(())` is returned, you **must** call `exit()` when done processing
    /// the node, or use `enter_guarded()` for automatic cleanup.
    pub fn enter(&mut self, node: T) -> Result<(), Cycle<T>> {
        if self.active.contains(&node) {
            // Found a cycle - build the cycle path
            let mut path = self.stack.clone();
            path.push(node.clone());

            // Find where the cycle starts
            let cycle_start = path
                .iter()
                .position(|n| n == &node)
                .expect("node must be in stack since it's in active set");

            return Err(Cycle { path, cycle_start });
        }

        self.active.insert(node.clone());
        self.stack.push(node);
        Ok(())
    }

    /// Exit the current node in the traversal.
    ///
    /// This should be called when done processing a node that was entered with `enter()`.
    ///
    /// # Panics
    ///
    /// Panics if called when no node is active (stack is empty).
    pub fn exit(&mut self) {
        let node = self
            .stack
            .pop()
            .expect("exit() called with empty stack - mismatched enter/exit");
        self.active.remove(&node);
    }

    /// Check if a node is currently being visited (in the active path).
    pub fn is_active(&self, node: &T) -> bool {
        self.active.contains(node)
    }

    /// Get the current traversal depth.
    pub fn depth(&self) -> usize {
        self.stack.len()
    }

    /// Get the current path being traversed.
    pub fn current_path(&self) -> &[T] {
        &self.stack
    }

    /// Check if the detector is empty (no active traversal).
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Clear the detector, resetting it to initial state.
    pub fn clear(&mut self) {
        self.active.clear();
        self.stack.clear();
    }
}

impl<T: Clone + Eq + Hash + Debug> Default for CycleDetector<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_cycle() {
        let mut detector = CycleDetector::new();

        assert!(detector.enter("A").is_ok());
        assert!(detector.enter("B").is_ok());
        assert!(detector.enter("C").is_ok());

        detector.exit();
        detector.exit();
        detector.exit();

        assert!(detector.is_empty());
    }

    #[test]
    fn test_self_cycle() {
        let mut detector = CycleDetector::new();

        detector.enter("A").unwrap();
        let result = detector.enter("A");

        assert!(result.is_err());
        let cycle = result.unwrap_err();
        assert!(cycle.is_self_cycle());
        assert_eq!(cycle.cycle_cause(), &"A");
        assert_eq!(cycle.path(), &["A", "A"]);
        assert_eq!(cycle.cycle(), &["A"]);
    }

    #[test]
    fn test_indirect_cycle() {
        let mut detector = CycleDetector::new();

        detector.enter("A").unwrap();
        detector.enter("B").unwrap();
        detector.enter("C").unwrap();
        let result = detector.enter("A");

        assert!(result.is_err());
        let cycle = result.unwrap_err();
        assert!(!cycle.is_self_cycle());
        assert_eq!(cycle.cycle_cause(), &"A");
        assert_eq!(cycle.path(), &["A", "B", "C", "A"]);
        assert_eq!(cycle.cycle(), &["A", "B", "C"]);
        assert_eq!(cycle.cycle_start_index(), 0);
    }

    #[test]
    fn test_cycle_in_middle() {
        let mut detector = CycleDetector::new();

        detector.enter("X").unwrap();
        detector.enter("A").unwrap();
        detector.enter("B").unwrap();
        detector.enter("C").unwrap();
        let result = detector.enter("A");

        assert!(result.is_err());
        let cycle = result.unwrap_err();
        assert_eq!(cycle.path(), &["X", "A", "B", "C", "A"]);
        assert_eq!(cycle.cycle(), &["A", "B", "C"]);
        assert_eq!(cycle.cycle_start_index(), 1);
    }

    #[test]
    fn test_reuse_after_exit() {
        let mut detector = CycleDetector::new();

        detector.enter("A").unwrap();
        detector.enter("B").unwrap();
        detector.exit();
        detector.exit();

        // Should be able to enter "A" again after exiting
        assert!(detector.enter("A").is_ok());
        assert!(detector.enter("B").is_ok());
    }

    #[test]
    fn test_is_active() {
        let mut detector = CycleDetector::new();

        assert!(!detector.is_active(&"A"));

        detector.enter("A").unwrap();
        assert!(detector.is_active(&"A"));
        assert!(!detector.is_active(&"B"));

        detector.enter("B").unwrap();
        assert!(detector.is_active(&"A"));
        assert!(detector.is_active(&"B"));

        detector.exit();
        assert!(detector.is_active(&"A"));
        assert!(!detector.is_active(&"B"));
    }

    #[test]
    fn test_current_path() {
        let mut detector = CycleDetector::new();

        assert_eq!(detector.current_path(), &[] as &[&str]);

        detector.enter("A").unwrap();
        assert_eq!(detector.current_path(), &["A"]);

        detector.enter("B").unwrap();
        assert_eq!(detector.current_path(), &["A", "B"]);

        detector.enter("C").unwrap();
        assert_eq!(detector.current_path(), &["A", "B", "C"]);

        detector.exit();
        assert_eq!(detector.current_path(), &["A", "B"]);
    }

    #[test]
    fn test_clear() {
        let mut detector = CycleDetector::new();

        detector.enter("A").unwrap();
        detector.enter("B").unwrap();

        detector.clear();

        assert!(detector.is_empty());
        assert!(!detector.is_active(&"A"));
        assert!(!detector.is_active(&"B"));
    }

    #[test]
    #[should_panic(expected = "exit() called with empty stack")]
    fn test_exit_without_enter_panics() {
        let mut detector: CycleDetector<&str> = CycleDetector::new();
        detector.exit();
    }
}
