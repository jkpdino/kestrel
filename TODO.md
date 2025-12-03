# Kestrel TODO

This file tracks immediate next steps and their dependencies.

## Current Priority: Binary Operators

## Operators

### 1. Binary Operators
**Status**: Not started

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

### 2. Comparison Operators
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

### 3. Logical Operators
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

### 4. If Expressions
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

### 5. While Loops
**Status**: Not started
**Depends on**: Comparison operators

```kestrel
while condition {
    body
}
```

**Implementation**:
- [ ] Parser: `while` keyword, condition, body block
- [ ] AST: `Statement::While { condition, body }`
- [ ] Semantic: Condition must be `Bool`

### 6. Return / Break / Continue
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

### 7. Type Checking
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
1. Binary Operators ──┬─> 2. Comparison ──┬─> 3. Logical
                      │                   │        │
                      │                   └────────┴─> 4. If Expressions
                      │                                      │
                      └─> 5. While Loops <───────────────────┘
                               │
                               └─> 6. Return/Break/Continue

7. Type Checking (incremental, alongside each feature)
```

**Next**:
1. Binary operators (+, -, *, /, %)
2. Comparison operators
3. Logical operators

**Then**:
4. If expressions
5. While loops
6. Return/break/continue

