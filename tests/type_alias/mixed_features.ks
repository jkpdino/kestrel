// Type aliases mixed with other features
module Graphics

// Import with type alias
import System.Collections

// Type aliases at module level
public type Color = RGB;
type Position = Point2D;

// Class with type aliases (if supported in class body)
class Shape {
    // Note: Type aliases in class bodies depend on whether
    // the parser allows them in DeclarationItemData
}

// Multiple type aliases in sequence
type First = A;
type Second = B;
type Third = C;
type Fourth = D;
type Fifth = E;

// Type aliases with different visibility levels
public type PublicType = Visible;
private type PrivateType = Hidden;
internal type InternalType = ModuleScoped;
fileprivate type FileType = FileScoped;

// Interleaved declarations
type Point = Coordinate;
import Math.Vectors
type Vector = Direction;
class Triangle {}
type Angle = Degree;

// Type aliases for the same target type
type Alias1 = TargetType;
type Alias2 = TargetType;
type Alias3 = TargetType;

// Type aliases chained (one aliasing another)
// Note: This tests that aliased types can be identifiers
type Base = Foundation;
type Derived = Base;
type Final = Derived;
