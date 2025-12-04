//! Tests for field access expressions.
//!
//! These tests verify that struct field access is correctly resolved
//! and that appropriate errors are raised for invalid field access.

use kestrel_test_suite::*;

mod field_access {
    use super::*;

    #[test]
    fn simple_field_access() {
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int
}

func getX(p: Point) -> Int {
    p.x
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn chained_field_access() {
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

func getStartX(line: Line) -> Int {
    line.start.x
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn field_access_in_variable() {
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int
}

func example(p: Point) -> Int {
    let val: Int = p.x;
    val
}
"#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nonexistent_field_error() {
        Test::new(
            r#"
module Main

struct Point {
    let x: Int
    let y: Int
}

func getZ(p: Point) -> Int {
    p.z
}
"#,
        )
        .expect(HasError("no member 'z' on type 'Point'"));
    }

    #[test]
    fn member_access_on_primitive_error() {
        Test::new(
            r#"
module Main

func test(x: Int) -> Int {
    x.foo
}
"#,
        )
        .expect(HasError("cannot access member on type"));
    }

    #[test]
    fn private_field_access_error() {
        Test::new(
            r#"
module Main

struct Secret {
    private let hidden: Int
}

func peek(s: Secret) -> Int {
    s.hidden
}
"#,
        )
        .expect(HasError("is private"));
    }

    #[test]
    fn public_field_access_succeeds() {
        Test::new(
            r#"
module Main

struct Point {
    pub let x: Int
    pub let y: Int
}

func getX(p: Point) -> Int {
    p.x
}
"#,
        )
        .expect(Compiles);
    }
}

