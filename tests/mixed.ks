// Mixed declarations: modules, imports, and classes

module Main

import A.B.C
import X.Y as Z
import Foo.Bar.(one, two, three as renamed)

class MyClass { }

public class Container {
    class Nested { }
}
