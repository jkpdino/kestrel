# Kestrel TODO

This file tracks immediate next steps for Phase 5.

## Current Priority: Phase 5 Complete

---

## Phase 5: Validation & Type Checking

### Never Type Propagation
**Status**: DONE ✓

- [x] `Ty::join()` handles Never propagation in if expressions
- [x] `ElseBranch::ty()` helper for getting else branch type
- [x] If expressions properly compute type when branches contain Never

### Type Checking
**Status**: DONE ✓

Full type validation across the language.

**Implementation**:
- [x] `Ty::is_assignable_to()` - Type comparison with alias expansion
- [x] `Ty::expand_aliases()` - Follow type alias chains
- [x] `TypeCheckValidator` - Validator for all type checks
- [x] `TypeMismatchError` - Diagnostic for type errors
- [x] `ConditionNotBoolError` - Diagnostic for non-bool conditions
- [x] `BranchTypeMismatchError` - Diagnostic for if branch type mismatch
- [x] `ArrayElementTypeMismatchError` - Diagnostic for array element type mismatch

**Checks implemented**:
- [x] Return type checking
  - [x] `return expr` type matches function's declared return type
  - [x] Bare `return` only in functions returning `()`
  - [x] Yield expression (implicit return) type checking
- [x] Assignment type checking
  - [x] Value type matches target type
- [x] Variable binding type checking
  - [x] Initializer type matches declared type
- [x] Call argument type checking
  - [x] Argument types match parameter types (functions, methods, initializers)
- [x] If/while condition checking
  - [x] Condition must be Bool
- [x] If branch type checking
  - [x] Branch types must match when used as expression
- [x] Array literal type checking
  - [x] All elements must have same type
- [x] Struct nominal equality
  - [x] Different structs are incompatible even with same shape
- [x] Generic struct type inference
  - [x] `create_generic_struct_type()` infers type args from field values

### Parser Fixes
**Status**: DONE ✓

- [x] Inline semicolon-separated field declarations in structs
  - [x] `struct Point { var x: Int; var y: Int }` now parses correctly
  - [x] Optional trailing semicolon on field declarations

### Tuple Indexing
**Status**: DONE ✓

Access tuple elements by index: `tuple.0`, `tuple.1`

**Implementation**:
- [x] `SyntaxKind::ExprTupleIndex` - New syntax node for tuple indexing
- [x] Parser support for integer member access (`.0`, `.1`, etc.)
- [x] `ExprKind::TupleIndex` - Semantic expression variant
- [x] `resolve_tuple_index_expression()` - Resolution function
- [x] `TupleIndexOutOfBoundsError` - Index exceeds tuple length
- [x] `TupleIndexOnNonTupleError` - Using tuple index on non-tuple type
- [x] Mutability support (assignment to tuple elements)
- [x] Integration with validation passes

**Known Limitation**: Chained tuple access (`t.0.1`) is currently lexed as `t.0` followed by float `.1` due to lexer ambiguity. Use intermediate variables as a workaround.

**Example**:
```kestrel
let pair: (Int, String) = (42, "hello")
let x: Int = pair.0      // 42
let y: String = pair.1   // "hello"

var mutable = (1, 2)
mutable.0 = 10           // Assignment works
```

---

## Notes

- Type aliases are expanded for comparison
- No implicit coercions (`Int` ≠ `Float`)
- Generic constraint enforcement deferred to Phase 6
- Type parameter types are treated as compatible with anything (deferred to Phase 6)
- `Self` type is compatible with the containing struct/protocol type
