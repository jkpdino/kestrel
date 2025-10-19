# Types

Types are used in many different places. Because the type keyword is reserved in rust, we use ty, Ty, and TyKind. All comma separated lists can have trailing commas

## Syntactic Types

```
// Unit Type
()
// Never Type
!
// Tuple Type
(type1, type2, etc,)
// Function Type
(param1, param2, etc,) -> return_type
// Path Type
// This type should be parsed, but we will resolve it before the semantics phase
Ident.Ident.Ident
```

## Semantic Types

### Class Type

Class(Arc<ClassSymbol>)
