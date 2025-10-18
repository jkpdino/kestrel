The module of a file is declared as follows

```
module A.B.C

// rest of file
```

Exactly one module declaration must be given per file

If none are given, throw an error at the first declaration in the file.
If multiple are given, throw an error at each declaration in the file.
