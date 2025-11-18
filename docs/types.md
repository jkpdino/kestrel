# Type Aliases

Type aliases provide a way to alias types. They are declared like this:

```
<visibility> type Alias = Aliased;
```

## Implementation

### Build Phase

Type aliases are created during the build phase with two `TypedBehavior` instances:

1. **Syntactic TypedBehavior**: Contains the aliased type as parsed from the source code. This may contain unresolved `Path` type variants (e.g., `A.B.C`).

2. **Semantic TypedBehavior**: Contains `TyKind::TypeAlias` variant that references the type alias symbol itself. This allows type checkers to distinguish type aliases from their underlying types.

### Binding Phase

During the binding phase, the `TypeAliasResolver` implements `bind_declaration` to:

1. Extract the syntactic aliased type from the first `TypedBehavior`
2. Resolve all `Path` variants in the type using `resolve_type()` from `type_resolver.rs`
3. Create a `TypeAliasTypedBehavior` with the fully resolved type
4. Add this behavior to the symbol

### Type Resolution

After binding, a type alias symbol has three behaviors with type information:

1. **TypedBehavior (syntactic)**: The original aliased type with possibly unresolved paths
2. **TypedBehavior (semantic)**: The `TypeAlias` type variant
3. **TypeAliasTypedBehavior**: The fully resolved aliased type

When a type checker encounters a `TyKind::TypeAlias`, it can look up the symbol's `TypeAliasTypedBehavior` to get the resolved underlying type.

## TyKind Variants

The `TyKind` enum includes a `TypeAlias` variant:

```rust
TypeAlias(Arc<TypeAliasSymbol>)
```

This variant represents a reference to a type alias before it has been replaced with its underlying type during type checking.
