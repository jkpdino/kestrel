use kestrel_test_suite::*;

mod basic {
    use super::*;

    #[test]
    fn empty_struct() {
        Test::new("module Test\nstruct Foo {}")
            .expect(Compiles)
            .expect(Symbol::new("Foo").is(SymbolKind::Struct));
    }

    #[test]
    fn public_struct() {
        Test::new("module Test\npublic struct Bar {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Bar")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Public)),
            );
    }

    #[test]
    fn private_struct() {
        Test::new("module Test\nprivate struct Baz {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Baz")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Private)),
            );
    }

    #[test]
    fn internal_struct() {
        Test::new("module Test\ninternal struct Qux {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Qux")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Internal)),
            );
    }

    #[test]
    fn fileprivate_struct() {
        Test::new("module Test\nfileprivate struct Quux {}")
            .expect(Compiles)
            .expect(
                Symbol::new("Quux")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Fileprivate)),
            );
    }

    #[test]
    fn default_visibility_is_internal() {
        Test::new("module Test\nstruct DefaultVis {}")
            .expect(Compiles)
            .expect(
                Symbol::new("DefaultVis")
                    .is(SymbolKind::Struct)
                    .has(Behavior::Visibility(Visibility::Internal)),
            );
    }

    #[test]
    fn multiple_structs() {
        Test::new(
            r#"module Test
            struct First {}
            struct Second {}
            struct Third {}
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("First").is(SymbolKind::Struct))
        .expect(Symbol::new("Second").is(SymbolKind::Struct))
        .expect(Symbol::new("Third").is(SymbolKind::Struct));
    }
}

mod nested {
    use super::*;

    #[test]
    fn nested_struct() {
        Test::new(
            r#"module Test
            struct Outer {
                struct Inner {}
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Outer").is(SymbolKind::Struct))
        .expect(Symbol::new("Inner").is(SymbolKind::Struct));
    }

    #[test]
    fn deeply_nested_structs() {
        Test::new(
            r#"module Test
            struct Level1 {
                struct Level2 {
                    struct Level3 {}
                }
            }
        "#,
        )
        .expect(Compiles)
        .expect(Symbol::new("Level1").is(SymbolKind::Struct))
        .expect(Symbol::new("Level2").is(SymbolKind::Struct))
        .expect(Symbol::new("Level3").is(SymbolKind::Struct));
    }
}

// TODO: Enable once assignment expressions are implemented
// mod initializers {
//     use super::*;
//
//     #[test]
//     fn explicit_initializer_parses() {
//         Test::new(
//             r#"module Test
//             struct Point {
//                 var x: Int
//                 var y: Int
//
//                 init(x: Int, y: Int) {
//                     self.x = x
//                     self.y = y
//                 }
//             }
//         "#,
//         )
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn multiple_initializers() {
//         Test::new(
//             r#"module Test
//             struct Point {
//                 var x: Int
//                 var y: Int
//
//                 init(x: Int, y: Int) {
//                     self.x = x
//                     self.y = y
//                 }
//
//                 init() {
//                     self.x = 0
//                     self.y = 0
//                 }
//             }
//         "#,
//         )
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn initializer_with_visibility() {
//         Test::new(
//             r#"module Test
//             struct Point {
//                 var x: Int
//
//                 public init(x: Int) {
//                     self.x = x
//                 }
//             }
//         "#,
//         )
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn initializer_without_params() {
//         Test::new(
//             r#"module Test
//             struct Counter {
//                 var count: Int
//
//                 init() {
//                     self.count = 0
//                 }
//             }
//         "#,
//         )
//         .expect(Compiles);
//     }
// }

mod instantiation {
    use super::*;

    #[test]
    fn implicit_struct_instantiation() {
        Test::new(
            r#"module Test
            struct Point {
                var x: Int
                var y: Int
            }

            func makePoint() -> Point {
                Point(x: 10, y: 20)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn empty_struct_instantiation() {
        Test::new(
            r#"module Test
            struct Empty {}

            func makeEmpty() -> Empty {
                Empty()
            }
        "#,
        )
        .expect(Compiles);
    }

    // TODO: Enable once assignment expressions are implemented
    // #[test]
    // fn explicit_init_instantiation() {
    //     Test::new(
    //         r#"module Test
    //         struct Point {
    //             var x: Int
    //             var y: Int
    //
    //             init(x: Int, y: Int) {
    //                 self.x = x
    //                 self.y = y
    //             }
    //         }
    //
    //         func makePoint() -> Point {
    //             Point(x: 5, y: 10)
    //         }
    //     "#,
    //     )
    //     .expect(Compiles);
    // }

    // TODO: Enable once assignment expressions are implemented
    // #[test]
    // fn explicit_init_with_different_labels() {
    //     Test::new(
    //         r#"module Test
    //         struct Point {
    //             var x: Int
    //             var y: Int
    //
    //             init(atX x: Int, atY y: Int) {
    //                 self.x = x
    //                 self.y = y
    //             }
    //         }
    //
    //         func makePoint() -> Point {
    //             Point(atX: 5, atY: 10)
    //         }
    //     "#,
    //     )
    //     .expect(Compiles);
    // }

    // TODO: Enable once assignment expressions are implemented
    // #[test]
    // fn multiple_init_overloads() {
    //     Test::new(
    //         r#"module Test
    //         struct Point {
    //             var x: Int
    //             var y: Int
    //
    //             init(x: Int, y: Int) {
    //                 self.x = x
    //                 self.y = y
    //             }
    //
    //             init() {
    //                 self.x = 0
    //                 self.y = 0
    //             }
    //
    //             init(value: Int) {
    //                 self.x = value
    //                 self.y = value
    //             }
    //         }
    //
    //         func test() {
    //             let p1 = Point(x: 1, y: 2)
    //             let p2 = Point()
    //             let p3 = Point(value: 5)
    //         }
    //     "#,
    //     )
    //     .expect(Compiles);
    // }

    #[test]
    fn single_field_struct() {
        Test::new(
            r#"module Test
            struct Wrapper {
                var value: Int
            }

            func wrap() -> Wrapper {
                Wrapper(value: 42)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn nested_struct_instantiation() {
        Test::new(
            r#"module Test
            struct Inner {
                var value: Int
            }

            struct Outer {
                var inner: Inner
            }

            func makeOuter() -> Outer {
                Outer(inner: Inner(value: 42))
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn struct_with_many_fields() {
        Test::new(
            r#"module Test
            struct BigStruct {
                var a: Int
                var b: Int
                var c: Int
                var d: Int
                var e: Int
            }

            func makeBig() -> BigStruct {
                BigStruct(a: 1, b: 2, c: 3, d: 4, e: 5)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn instantiation_in_variable_binding() {
        Test::new("module Test\nstruct Point { var x: Int\n var y: Int }\nfunc test() -> Point { let p: Point = Point(x: 1, y: 2); p }")
        .expect(Compiles);
    }

    #[test]
    fn instantiation_as_function_argument() {
        // Note: `p: Point` means parameter named p with type Point, no external label
        Test::new("module Test\nstruct Point { var x: Int\n var y: Int }\nfunc takePoint(p: Point) -> Int { 42 }\nfunc test() -> Int { takePoint(Point(x: 1, y: 2)) }")
        .expect(Compiles);
    }

    #[test]
    fn instantiation_with_let_fields() {
        Test::new(
            r#"module Test
            struct Immutable {
                let x: Int
                let y: Int
            }

            func make() -> Immutable {
                Immutable(x: 1, y: 2)
            }
        "#,
        )
        .expect(Compiles);
    }

    #[test]
    fn instantiation_mixed_let_var_fields() {
        Test::new(
            r#"module Test
            struct Mixed {
                let id: Int
                var value: Int
            }

            func make() -> Mixed {
                Mixed(id: 1, value: 2)
            }
        "#,
        )
        .expect(Compiles);
    }
}

mod instantiation_errors {
    use super::*;

    #[test]
    fn calling_non_struct_like_struct() {
        // This one should work - calling a function with wrong labels should error
        Test::new("module Test\nfunc notAStruct() -> Int { 42 }\nfunc test() -> Int { notAStruct(x: 1) }")
        .expect(HasError("no matching overload"));
    }

    #[test]
    fn wrong_label_order() {
        // Labels must match field declaration order for implicit init
        Test::new("module Test\nstruct Point { var x: Int\n var y: Int }\nfunc test() -> Point { Point(y: 2, x: 1) }")
        .expect(HasError(""));
    }

    #[test]
    fn missing_label() {
        Test::new("module Test\nstruct Point { var x: Int\n var y: Int }\nfunc test() -> Point { Point(1, 2) }")
        .expect(HasError(""));
    }

    #[test]
    fn wrong_arity_too_few() {
        Test::new("module Test\nstruct Point { var x: Int\n var y: Int }\nfunc test() -> Point { Point(x: 1) }")
        .expect(HasError(""));
    }

    #[test]
    fn wrong_arity_too_many() {
        Test::new("module Test\nstruct Point { var x: Int\n var y: Int }\nfunc test() -> Point { Point(x: 1, y: 2, z: 3) }")
        .expect(HasError(""));
    }

    #[test]
    fn wrong_label_name() {
        Test::new("module Test\nstruct Point { var x: Int\n var y: Int }\nfunc test() -> Point { Point(a: 1, b: 2) }")
        .expect(HasError(""));
    }

    // TODO: Enable once assignment expressions are implemented
    // #[test]
    // fn explicit_init_suppresses_implicit() {
    //     // When explicit init exists, implicit memberwise init should not be available
    //     Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n init() { self.x = 0; self.y = 0 } }\nfunc test() -> Point { Point(x: 1, y: 2) }")
    //     .expect(HasError(""));
    // }

    // TODO: Enable once assignment expressions are implemented
    // #[test]
    // fn no_matching_init_overload() {
    //     Test::new(r#"module Test
    //         struct Point {
    //             var x: Int
    //             var y: Int
    //             init(x: Int, y: Int) { self.x = x; self.y = y }
    //         }
    //         func test() -> Point { Point() }
    //     "#)
    //     .expect(HasError(""));
    // }
}

// TODO: Enable once assignment expressions are implemented
// mod initializer_edge_cases {
//     use super::*;
//
//     #[test]
//     fn init_with_self_access() {
//         Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n init(x: Int, y: Int) { self.x = x; self.y = y } }")
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn init_calling_other_method_after_init() {
//         Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n init(x: Int, y: Int) { self.x = x; self.y = y }\n func sum() -> Int { self.x } }")
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn init_with_complex_expressions() {
//         Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n init(x: Int, y: Int) { self.x = x; self.y = y } }\nfunc getInt() -> Int { 42 }\nfunc test() -> Point { Point(x: getInt(), y: getInt()) }")
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn private_init() {
//         Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n private init(x: Int, y: Int) { self.x = x; self.y = y }\n public static func origin() -> Point { Point(x: 0, y: 0) } }")
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn init_parameter_shadows_field() {
//         Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n init(x: Int, y: Int) { self.x = x; self.y = y } }")
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn init_with_different_param_names() {
//         Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n init(xVal: Int, yVal: Int) { self.x = xVal; self.y = yVal } }\nfunc test() -> Point { Point(xVal: 1, yVal: 2) }")
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn init_with_labeled_params() {
//         Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n init(at x: Int, and y: Int) { self.x = x; self.y = y } }\nfunc test() -> Point { Point(at: 1, and: 2) }")
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn empty_init_body() {
//         Test::new("module Test\nstruct Empty { init() { } }\nfunc test() -> Empty { Empty() }")
//         .expect(Compiles);
//     }
//
//     #[test]
//     fn init_with_local_variables() {
//         Test::new("module Test\nstruct Point { var x: Int\n var y: Int\n init(value: Int) { let doubled: Int = value; self.x = doubled; self.y = doubled } }")
//         .expect(Compiles);
//     }
// }
