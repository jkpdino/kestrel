# Kestrel TODO

This file tracks immediate next steps and their dependencies.

## Current Priority: Control Flow

## Operators

### 1. Binary Operators
**Status**: ✅ Complete

```kestrel
a + b
a - b
a * b
a / b
a % b
```

**Implementation**:
- [x] Parser: Binary expression with precedence (Pratt parsing)
- [x] AST: `ExprBinary` syntax node
- [x] Semantic: Desugar to method calls (`a + b` → `a.add(b)`)
- [x] Semantic: Primitive method lookup for Int, Float types
- [x] Semantic: Bitwise operators (`&`, `|`, `^`, `<<`, `>>`)

**Design Decision**: Operator overloading?
- Current: Built-in operators on primitive types via `PrimitiveMethod`
- Later: Protocol-based overloading (`Addable`, `Comparable`, etc.)

### 2. Comparison Operators
**Status**: ✅ Complete
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
- [x] Parser: Same infrastructure as binary operators
- [x] Semantic: Desugar to `eq`, `ne`, `lt`, `gt`, `le`, `ge` methods
- [x] Semantic: Primitive methods for Int, Float, Bool, String

### 3. Logical Operators
**Status**: ✅ Complete
**Depends on**: Comparison operators (for useful conditions)

```kestrel
a and b
a or b
not a
```

**Implementation**:
- [x] Parser: `and`/`or` as binary, `not` as unary prefix
- [x] Semantic: Desugar to `logicalAnd`, `logicalOr`, `logicalNot` methods
- [x] Semantic: Primitive methods on Bool type
- [ ] Semantic: Short-circuit evaluation (later, for codegen)

### 4. Unary Operators
**Status**: ✅ Complete

```kestrel
-x      // negation
+x      // identity
!x      // bitwise not (prefix) / unwrap (postfix)
not x   // logical not
```

**Implementation**:
- [x] Parser: Prefix and postfix unary operators
- [x] Semantic: Desugar to `neg`, `identity`, `bitNot`, `logicalNot` methods
- [x] Semantic: Primitive methods for Int, Float, Bool

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
1. Binary Operators ──┬─> 2. Comparison ──┬─> 3. Logical     ✅ DONE
                      │                   │        │
                      │                   └────────┴─> 4. If Expressions
                      │                                      │
                      └─> 5. While Loops <───────────────────┘
                               │
                               └─> 6. Return/Break/Continue

7. Type Checking (incremental, alongside each feature)
```

**Completed**:
1. ✅ Binary operators (+, -, *, /, %, &, |, ^, <<, >>)
2. ✅ Comparison operators (==, !=, <, >, <=, >=)
3. ✅ Logical operators (and, or, not)
4. ✅ Unary operators (+, -, !, not, postfix !)

**Next**:
1. If expressions
2. While loops
3. Return/break/continue

