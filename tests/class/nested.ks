// Nested class declarations

// Basic nested class
class Outer {
    class Inner { }
}

// Multiple levels of nesting
class Level1 {
    class Level2 {
        class Level3 { }
    }
}

// Multiple nested classes in one parent
class Container {
    class FirstNested { }
    class SecondNested { }
    class ThirdNested { }
}

// Nested classes with visibility modifiers
public class PublicOuter {
    private class PrivateInner { }
    public class PublicInner { }
    internal class InternalInner { }
}

// Deep nesting
class A {
    class B {
        class C {
            class D {
                class E { }
            }
        }
    }
}

// Mixed nested and non-nested classes
class First { }

class Second {
    class NestedInSecond { }
}

class Third { }

class Fourth {
    class NestedOne { }
    class NestedTwo {
        class DeeplyNested { }
    }
}
