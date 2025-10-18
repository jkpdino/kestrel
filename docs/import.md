An import of a module is declared in one of the following ways

```
// imports all member of A.B.C
import A.B.C

// Imports C as D
import A.B.C as D

// Imports D and E from C
import A.B.C.(D, E)

// Aliases D to E, F to G
import A.B.C.(D as E, F as G)
```
