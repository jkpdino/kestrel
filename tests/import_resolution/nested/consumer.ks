// Test importing from nested modules
module NestedConsumer

// Import from top-level Math
import Math

// Import from nested Math.Geometry
import Math.Geometry

// Import specific items from Math.Geometry
import Math.Geometry.(Point, Circle)

// Import with alias
import Math.Algebra as Alg

class MyApp {}
