# Semantics

```
// all paths are scoped from the root

// adds all (visible) symbols from the module to the scope
import path.to.module

// adds Foo and Bar to the scope, assuming they are visible
import path.to.module.(Foo, Bar)

// adds a symbol Foo to the scope, referring to the module
import path.to.module as Foo

// adds a symbol FooBar referring to Foo, and Baz referring to Bar
import path.to.module.(Foo as FooBar, Bar as Baz)
```

# Errors

## Module Not Found

If the root path doesn't exist:

import path.to.module
^^^^ Module `path` not found

If it does:

import path.to.module
^^ Module `to` not found in `path`

## Can't import symbol of type ...

import path.to.container
^ can't import a symbol of type Struct

struct container {
^ symbol declared as a type here

## Can't find symbol .. in ...

import path.to.container (Foo)
^ can't find symbol Foo in path.to.container

## Conflict

import path.to.container (Foo)
^ symbol Foo conflicts with other symbol

struct Foo
^ Foo is declared here

OR

import path.to.container (Foo as Bar)
^ symbol Bar conflicts with other symbol

struct Bar
^ Bar is declared here
