# Kestrel TODO

This file tracks immediate next steps for Phase 5.

## Current Priority: Type Checking

---

## Phase 5: Validation & Type Checking

### Never Type Propagation
**Status**: Not started

Expressions with type `Never` should propagate correctly through the type system.

**Implementation**:
- [ ] `Never` is compatible with any type (it never produces a value)
- [ ] Expressions containing `Never` sub-expressions have type `Never`
- [ ] Control flow after `Never` expressions is unreachable

**Examples**:
```kestrel
let x: Int = return 5    // return has type Never, but this is valid
let y: Int = if cond { 1 } else { return 2 }  // else branch is Never
```

### Type Checking
**Status**: Not started

Full type validation across the language.

**Implementation**:
- [ ] Return type checking
  - [ ] `return expr` type matches function's declared return type
  - [ ] Bare `return` only in functions returning `()`
- [ ] Assignment type checking
  - [ ] Value type matches target type
  - [ ] Field assignment type checking
- [ ] Function argument type checking
  - [ ] Argument types match parameter types
  - [ ] Generic type argument validation
- [ ] Binary/unary operator type checking
  - [ ] Operand types are valid for the operator
  - [ ] Result type is correct
- [ ] If expression type checking
  - [ ] Condition is Bool
  - [ ] Branch types match (when used as expression)
- [ ] Type error diagnostics
  - [ ] Clear "expected X, found Y" messages
  - [ ] Show both locations (expected and actual)

---

## Suggested Order

1. **Never type propagation** - Foundation for control flow typing
2. **Return type checking** - Most impactful for catching bugs
3. **Assignment type checking** - Catch type mismatches at assignment
4. **Argument type checking** - Validate function calls
5. **Operator type checking** - Complete expression typing

---

## Notes

- Type checking should produce clear, actionable error messages
- Consider whether to allow implicit coercions (probably not initially)
- Generic type checking will need constraint enforcement (Phase 6)
