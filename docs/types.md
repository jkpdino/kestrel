# Type Aliases

Type aliases provide a way to alias types. They are declared like this:

```
<visibility> type Alias = Aliased;
```

Type aliases create a new symbol that has TypedBehavior. Then it is given the type of TypeAlias, which refers to the type alias.

Then later when resolving a type, TypeAlias is replaced with the type it refers to.
