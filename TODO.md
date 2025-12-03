# Kestrel TODO

This file tracks immediate next steps and their dependencies.

## Current Priority: Binary Operators

Assignment expressions are now implemented. Binary operators are next.

### 1. Assignment Expressions
**Status**: ✅ Complete

**What's done**:
- [x] Parser: `=` operator with expression on both sides (lowest precedence, right-associative)
- [x] AST: `ExprKind::Assignment { target, value }`
- [x] Type: Returns `Never` (assignment as expression, but unusable)
- [x] Validation: `AssignmentValidationPass` checks LHS is mutable (`var`, not `let`)
- [x] Validation: Errors for assigning to immutable variables
- [x] Validation: `is_self_expr` implemented to recognize `self`
- [x] Integration with initializer verification for `self.field = value`

**Remaining work**:
- [ ] Validate field mutability when assigning to `obj.field`

**Design Decision**: Assignment is an expression with type `Never`.
- `x = 5` works as a statement (Never value discarded)
- `y = (x = 5)` is a type error (can't assign Never)
- Right-associative, lowest precedence

---

## Operators

### 2. Binary Operators
**Status**: Not started
**Depends on**: Nothing (can be done now)

```kestrel
a + b
a - b
a * b
a / b
a % b
```

**Implementation**:
- [ ] Parser: Binary expression with precedence
- [ ] AST: `ExprKind::Binary { op, lhs, rhs }`
- [ ] Semantic: Type check operands (both must be numeric for arithmetic)
- [ ] Semantic: Result type inference (Int op Int -> Int, etc.)

**Design Decision**: Operator overloading?
- For now: Built-in operators on primitive types only
- Later: Protocol-based overloading (`Addable`, `Comparable`, etc.)

### 3. Comparison Operators
**Status**: Not started
**Depends on**: Binary operator infrastructure

```kestrel
a == b
a != b
a < b
a > b
a <= b
a >= b
```

**Implementation**:
- [ ] Parser: Same as binary, different precedence
- [ ] Semantic: Result type is always `Bool`
- [ ] Semantic: Operands must be comparable

### 4. Logical Operators
**Status**: Not started
**Depends on**: Comparison operators (for useful conditions)

```kestrel
a and b
a or b
not a
```

**Implementation**:
- [ ] Parser: `and`/`or` as binary, `not` as unary prefix
- [ ] Semantic: Operands must be `Bool`
- [ ] Semantic: Short-circuit evaluation (later, for codegen)

---

## Control Flow

### 5. If Expressions
**Status**: Not started
**Depends on**: Comparison operators, logical operators

```kestrel
if condition {
    thenBranch
} else {
    elseBranch
}
```

**Implementation**:
- [ ] Parser: `if` keyword, condition, braces, optional `else`
- [ ] AST: `ExprKind::If { condition, then_branch, else_branch }`
- [ ] Semantic: Condition must be `Bool`
- [ ] Semantic: Branch types must match (if used as expression)

**Design Decision**: If as expression or statement?
- Expression: `let x = if cond { 1 } else { 2 }`
- Requires both branches, types must match
- Recommendation: Expression (more powerful)

### 6. While Loops
**Status**: Not started
**Depends on**: Comparison operators, assignment (for useful loops)

```kestrel
while condition {
    body
}
```

**Implementation**:
- [ ] Parser: `while` keyword, condition, body block
- [ ] AST: `Statement::While { condition, body }`
- [ ] Semantic: Condition must be `Bool`

### 7. Return / Break / Continue
**Status**: Not started
**Depends on**: Control flow (while, for)

```kestrel
return value
break
continue
```

**Implementation**:
- [ ] Parser: Keywords with optional expression (return)
- [ ] AST: `Statement::Return`, `Statement::Break`, `Statement::Continue`
- [ ] Semantic: Return type must match function signature
- [ ] Semantic: Break/continue only valid inside loops

---

## Type System

### 8. Type Checking
**Status**: Partial (some validation exists)
**Depends on**: All expression types being resolvable

**Current state**:
- Types are resolved for declarations
- Some expression types are inferred
- No systematic type checking pass

**Needs**:
- [ ] Unify expected vs actual types
- [ ] Type errors with good diagnostics
- [ ] Coercion rules (if any)
- [ ] Generic type argument inference

---

## Suggested Implementation Order

```
1. Assignment ────────────────────────────────────── ✅ DONE
                                                   │
2. Binary Operators ──┬─> 3. Comparison ──┬─> 4. Logical
                      │                   │        │
                      │                   └────────┴─> 5. If Expressions
                      │                                      │
                      └─> 6. While Loops <───────────────────┘
                               │
                               └─> 7. Return/Break/Continue

8. Type Checking (incremental, alongside each feature)
```

**Next batch** (can be parallelized):
1. Binary operators (+, -, *, /, %)
2. LHS validation for assignment

**After that**:
3. Comparison operators
4. Logical operators

**Then**:
5. If expressions
6. While loops
7. Return/break/continue

