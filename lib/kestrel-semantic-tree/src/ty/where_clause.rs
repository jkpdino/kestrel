use semantic_tree::symbol::SymbolId;

use super::Ty;

/// Represents a where clause containing type constraints.
///
/// Where clauses appear on generic containers (structs, functions, protocols, type aliases)
/// and specify bounds that type parameters must satisfy.
///
/// Example: `where T: Comparable[T] and Hashable, U: Display`
#[derive(Debug, Clone, Default)]
pub struct WhereClause {
    pub constraints: Vec<Constraint>,
}

impl WhereClause {
    /// Create an empty where clause
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
        }
    }

    /// Create a where clause with constraints
    pub fn with_constraints(constraints: Vec<Constraint>) -> Self {
        Self { constraints }
    }

    /// Check if the where clause has no constraints
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    /// Add a constraint to the where clause
    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    /// Get all bounds for a specific type parameter
    pub fn bounds_for(&self, param_id: SymbolId) -> Vec<&Ty> {
        self.constraints
            .iter()
            .filter_map(|c| match c {
                Constraint::TypeBound { param, bounds } if *param == param_id => Some(bounds),
                Constraint::TypeBound { .. } => None,
            })
            .flatten()
            .collect()
    }
}

/// A single constraint in a where clause.
#[derive(Debug, Clone)]
pub enum Constraint {
    /// A type bound constraint: `T: Protocol and Protocol2`
    ///
    /// The `param` is the SymbolId of the type parameter being constrained.
    /// The `bounds` are the types that the parameter must satisfy (typically protocols,
    /// but can be generic protocol instantiations like `Iterator[Int]`).
    TypeBound {
        /// The SymbolId of the type parameter being constrained
        param: SymbolId,
        /// The bounds that the type parameter must satisfy
        bounds: Vec<Ty>,
    },
    // Future: TypeEquality for associated types
    // TypeEquality { left: TypePath, right: Ty }
}

impl Constraint {
    /// Create a new type bound constraint
    pub fn type_bound(param: SymbolId, bounds: Vec<Ty>) -> Self {
        Constraint::TypeBound { param, bounds }
    }

    /// Get the type parameter this constraint applies to
    pub fn param_id(&self) -> SymbolId {
        match self {
            Constraint::TypeBound { param, .. } => *param,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_where_clause() {
        let wc = WhereClause::new();
        assert!(wc.is_empty());
        assert!(wc.constraints.is_empty());
    }

    #[test]
    fn test_where_clause_with_constraints() {
        let param_id = SymbolId::new();
        let bound = Ty::path(vec!["Hashable".to_string()], 0..8);

        let constraint = Constraint::type_bound(param_id, vec![bound]);
        let wc = WhereClause::with_constraints(vec![constraint]);

        assert!(!wc.is_empty());
        assert_eq!(wc.constraints.len(), 1);

        let bounds = wc.bounds_for(param_id);
        assert_eq!(bounds.len(), 1);
    }

    #[test]
    fn test_bounds_for_unknown_param() {
        let param_id = SymbolId::new();
        let other_id = SymbolId::new();
        let bound = Ty::path(vec!["Hashable".to_string()], 0..8);

        let constraint = Constraint::type_bound(param_id, vec![bound]);
        let wc = WhereClause::with_constraints(vec![constraint]);

        // Looking for bounds on a different param
        let bounds = wc.bounds_for(other_id);
        assert!(bounds.is_empty());
    }
}
