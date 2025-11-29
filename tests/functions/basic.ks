module Functions.Basic

// Basic function with no parameters
fn empty() { }

// Function with single parameter
fn single(x: Int) { }

// Function with multiple parameters
fn multiple(a: Int, b: String, c: Float) { }

// Function with return type
fn withReturn() -> Int { }

// Function with parameters and return type
fn addNumbers(x: Int, y: Int) -> Int { }

// Public function
public fn publicFunc() { }

// Private function
private fn privateFunc() { }

// Internal function
internal fn internalFunc() { }

// Fileprivate function
fileprivate fn fileprivateFunc() { }

// Static function
static fn staticFunc() { }

// Public static function
public static fn publicStaticFunc() { }

// Function with labeled parameter (Swift-style)
fn greetPerson(with name: String) { }

// Function with multiple labeled parameters
fn sendMessage(from sender: String, to recipient: String) { }

// Function with mixed labeled and unlabeled parameters
fn mixedParams(label1 x: Int, y: Int, label2 z: String) { }

// Function returning unit type explicitly
fn returnsUnit() -> () { }

// Function returning never type
fn returnsNever() -> ! { }

// Function returning tuple
fn returnsTuple() -> (Int, String) { }

// Function returning function type
fn returnsFunction() -> (Int) -> String { }

// Function with function type parameter
fn takesFunction(f: (Int, Int) -> Bool) { }

// Function with complex nested type
fn complexTypes(f: ((Int) -> String, Bool) -> (Float, Float)) { }

// Function with path type parameter
fn pathType(point: Geometry.Point) { }
