module Functions.ValidOverloads

// ============================================================
// VALID OVERLOADS - These should NOT produce errors
// ============================================================

// --- Overloading by arity (different number of parameters) ---

func byArity() { }
func byArity(x: Int) { }
func byArity(x: Int, y: Int) { }
func byArity(x: Int, y: Int, z: Int) { }

// --- Overloading by parameter types ---

func byType(x: Int) { }
func byType(x: Float) { }
func byType(x: String) { }
func byType(x: Bool) { }

// Multiple parameters, different types
func byTypes(x: Int, y: Int) { }
func byTypes(x: Float, y: Float) { }
func byTypes(x: Int, y: Float) { }
func byTypes(x: Float, y: Int) { }

// --- Overloading by labels (labeled vs unlabeled) ---

func byLabel(x: Int) { }
func byLabel(with x: Int) { }
func byLabel(using x: Int) { }
func byLabel(from x: Int) { }

// Same types, different labels
func sendEmail(to recipient: String) { }
func sendEmail(from sender: String) { }
func sendEmail(recipient: String) { }

// Multiple labeled parameters, different label combinations
func transfer(from source: Int, to dest: Int) { }
func transfer(at location: Int, with value: Int) { }
func transfer(source: Int, dest: Int) { }

// --- Mixed overloading (arity + types) ---

func mixed(x: Int) { }
func mixed(x: Int, y: Int) { }
func mixed(x: Float) { }
func mixed(x: Float, y: Float) { }
func mixed(x: String, y: Int, z: Bool) { }

// --- Mixed overloading (types + labels) ---

func mixedTypeLabel(value x: Int) { }
func mixedTypeLabel(value x: Float) { }
func mixedTypeLabel(amount x: Int) { }
func mixedTypeLabel(amount x: Float) { }
func mixedTypeLabel(x: Int) { }

// --- Overloading with different return types (same params) ---
// Note: Return type alone doesn't distinguish overloads,
// but different param types with different returns is fine

func withReturns(x: Int) -> Int { }
func withReturns(x: Float) -> Float { }
func withReturns(x: String) -> Bool { }

// --- Overloading with complex types ---

func complexOverload(f: (Int) -> String) { }
func complexOverload(f: (Float) -> String) { }
func complexOverload(f: (Int) -> Int) { }
func complexOverload(f: (Int, Int) -> String) { }

// --- Overloading with tuple types ---

func tupleOverload(t: (Int, Int)) { }
func tupleOverload(t: (Int, String)) { }
func tupleOverload(t: (String, String)) { }
func tupleOverload(t: (Int, Int, Int)) { }

// --- Visibility doesn't affect overloading ---

public func visibilityTest(x: Int) { }
private func visibilityTest(x: Float) { }
internal func visibilityTest(x: String) { }

// --- Static doesn't affect overloading signature ---

func staticTest(x: Int) { }
static func staticTest(x: Float) { }
public static func staticTest(x: String) { }

// --- Path types as different types ---

func pathOverload(p: Module.TypeA) { }
func pathOverload(p: Module.TypeB) { }
func pathOverload(p: Other.TypeA) { }
