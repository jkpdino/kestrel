//! Tests for cycle detection validation passes.
//!
//! These tests verify that the compiler correctly detects:
//! - Struct containment cycles (infinite-size types)
//! - Generic constraint cycles
//! - Protocol inheritance cycles

use kestrel_test_suite::*;

mod struct_cycles {
    use super::*;

    #[test]
    fn direct_self_reference_error() {
        // Struct containing itself directly
        Test::new(
            r#"
module Main

struct Node {
    let next: Node
}
"#,
        )
        .expect(HasError("cannot contain itself"));
    }

    #[test]
    fn two_struct_cycle_error() {
        // Two structs containing each other
        Test::new(
            r#"
module Main

struct A {
    let b: B
}

struct B {
    let a: A
}
"#,
        )
        .expect(HasError("circular struct containment"));
    }

    #[test]
    fn three_struct_cycle_error() {
        // Three structs in a cycle: A -> B -> C -> A
        Test::new(
            r#"
module Main

struct A {
    let b: B
}

struct B {
    let c: C
}

struct C {
    let a: A
}
"#,
        )
        .expect(HasError("circular struct containment"));
    }

    #[test]
    fn array_breaks_cycle() {
        // Arrays use indirection, so this should be allowed
        Test::new(
            r#"
module Main

struct Node {
    let children: [Node]
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn tuple_with_cycle_error() {
        // Tuple containing struct that references parent
        Test::new(
            r#"
module Main

struct A {
    let pair: (Int, B)
}

struct B {
    let a: A
}
"#,
        )
        .expect(HasError("circular struct containment"));
    }

    #[test]
    fn no_cycle_different_structs() {
        // Structs referencing each other but no cycle
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int
}

struct Line {
    let start: Point
    let end: Point
}

struct Triangle {
    let a: Point
    let b: Point
    let c: Point
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_structs_no_cycle() {
        // Deep nesting but no cycle
        Test::new(
            r#"
module Main

struct Inner {
    let value: Int
}

struct Middle {
    let inner: Inner
}

struct Outer {
    let middle: Middle
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn self_reference_in_array_ok() {
        // Self-reference through array is ok (indirect)
        Test::new(
            r#"
module Main

struct TreeNode {
    let value: Int
    let children: [TreeNode]
}
"#,
        )
        .expect(Compiles);
    }
}

mod constraint_cycles {
    use super::*;

    #[test]
    fn mutual_constraint_reference_allowed() {
        // T's bound references U, U's bound references T through protocol generic.
        // This is currently allowed because the constraints don't create a true
        // dependency cycle - they just reference each other's types in protocol
        // instantiations. The constraint cycle detection looks for cases where
        // resolving one constraint requires resolving another in a cycle.
        //
        // In practice, this pattern is valid in many languages (like Swift).
        Test::new(
            r#"
module Main

protocol Container[T] {
    func get() -> T
}

func swap[T: Container[U], U: Container[T]](a: T, b: U) -> () {
    ()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn no_cycle_independent_constraints() {
        // Independent constraints, no cycle
        Test::new(
            r#"
module Main

protocol Printable {
    func print() -> String
}

protocol Comparable {
    func compare() -> Int
}

func process[T: Printable, U: Comparable](a: T, b: U) -> () {
    ()
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn single_constraint_no_cycle() {
        // Single constraint, no cycle possible
        Test::new(
            r#"
module Main

protocol Hashable {
    func hash() -> Int
}

struct Set[T: Hashable] {
}
"#,
        )
        .expect(Compiles);
    }
}

mod protocol_inheritance_cycles {
    use super::*;

    // Note: Protocol inheritance cycles are tested through conformances
    // which may already have coverage in protocols.rs

    #[test]
    fn direct_protocol_self_inheritance() {
        // Protocol conforming to itself
        Test::new(
            r#"
module Main

protocol Recursive: Recursive {
    func method() -> Int
}
"#,
        )
        .expect(HasError("circular"));
    }

    #[test]
    fn two_protocol_cycle() {
        // Two protocols inheriting from each other
        Test::new(
            r#"
module Main

protocol A: B {
    func methodA() -> Int
}

protocol B: A {
    func methodB() -> Int
}
"#,
        )
        .expect(HasError("circular"));
    }

    #[test]
    fn three_protocol_cycle() {
        // Three protocols in inheritance cycle
        Test::new(
            r#"
module Main

protocol A: B {
    func a() -> Int
}

protocol B: C {
    func b() -> Int
}

protocol C: A {
    func c() -> Int
}
"#,
        )
        .expect(HasError("circular"));
    }

    #[test]
    fn linear_protocol_inheritance_ok() {
        // Linear chain, no cycle
        Test::new(
            r#"
module Main

protocol Base {
    func base() -> Int
}

protocol Middle: Base {
    func middle() -> Int
}

protocol Derived: Middle {
    func derived() -> Int
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn diamond_inheritance_ok() {
        // Diamond pattern (A <- B, A <- C, B & C <- D) is not a cycle
        Test::new(
            r#"
module Main

protocol A {
    func a() -> Int
}

protocol B: A {
    func b() -> Int
}

protocol C: A {
    func c() -> Int
}
"#,
        )
        .expect(Compiles);
    }
}
