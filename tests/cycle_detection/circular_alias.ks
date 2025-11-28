// Direct circular reference: A -> B -> A
module CycleTest

type A = B;
type B = A;
