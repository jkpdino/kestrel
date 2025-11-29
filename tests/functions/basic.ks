module Functions.Basic

// Basic function with no parameters
func empty() { }

// Function with single parameter
func single(x: Int) { }

// Function with multiple parameters
func multiple(a: Int, b: String, c: Float) { }

// Function with return type
func withReturn() -> Int { }

// Function with parameters and return type
func addNumbers(x: Int, y: Int) -> Int { }

// Public function
public func publicFunc() { }

// Private function
private func privateFunc() { }

// Internal function
internal func internalFunc() { }

// Fileprivate function
fileprivate func fileprivateFunc() { }

// Static function
static func staticFunc() { }

// Public static function
public static func publicStaticFunc() { }

// Function with labeled parameter (Swift-style)
func greetPerson(with name: String) { }

// Function with multiple labeled parameters
func sendMessage(from sender: String, to recipient: String) { }

// Function with mixed labeled and unlabeled parameters
func mixedParams(label1 x: Int, y: Int, label2 z: String) { }

// Function returning unit type explicitly
func returnsUnit() -> () { }

// Function returning never type
func returnsNever() -> ! { }

// Function returning tuple
func returnsTuple() -> (Int, String) { }

// Function returning function type
func returnsFunction() -> (Int) -> String { }

// Function with function type parameter
func takesFunction(f: (Int, Int) -> Bool) { }

// Function with complex nested type
func complexTypes(f: ((Int) -> String, Bool) -> (Float, Float)) { }

// Function with path type parameter
func pathType(point: Geometry.Point) { }
