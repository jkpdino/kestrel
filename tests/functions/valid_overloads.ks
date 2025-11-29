module Functions.ValidOverloads

// ============================================================
// VALID OVERLOADS - These should NOT produce errors
// ============================================================

// --- Overloading by arity (different number of parameters) ---

fn byArity() { }
fn byArity(x: Int) { }
fn byArity(x: Int, y: Int) { }
fn byArity(x: Int, y: Int, z: Int) { }

// --- Overloading by parameter types ---

fn byType(x: Int) { }
fn byType(x: Float) { }
fn byType(x: String) { }
fn byType(x: Bool) { }

// Multiple parameters, different types
fn byTypes(x: Int, y: Int) { }
fn byTypes(x: Float, y: Float) { }
fn byTypes(x: Int, y: Float) { }
fn byTypes(x: Float, y: Int) { }

// --- Overloading by labels (labeled vs unlabeled) ---

fn byLabel(x: Int) { }
fn byLabel(with x: Int) { }
fn byLabel(using x: Int) { }
fn byLabel(from x: Int) { }

// Same types, different labels
fn sendEmail(to recipient: String) { }
fn sendEmail(from sender: String) { }
fn sendEmail(recipient: String) { }

// Multiple labeled parameters, different label combinations
fn transfer(from source: Int, to dest: Int) { }
fn transfer(at location: Int, with value: Int) { }
fn transfer(source: Int, dest: Int) { }

// --- Mixed overloading (arity + types) ---

fn mixed(x: Int) { }
fn mixed(x: Int, y: Int) { }
fn mixed(x: Float) { }
fn mixed(x: Float, y: Float) { }
fn mixed(x: String, y: Int, z: Bool) { }

// --- Mixed overloading (types + labels) ---

fn mixedTypeLabel(value x: Int) { }
fn mixedTypeLabel(value x: Float) { }
fn mixedTypeLabel(amount x: Int) { }
fn mixedTypeLabel(amount x: Float) { }
fn mixedTypeLabel(x: Int) { }

// --- Overloading with different return types (same params) ---
// Note: Return type alone doesn't distinguish overloads,
// but different param types with different returns is fine

fn withReturns(x: Int) -> Int { }
fn withReturns(x: Float) -> Float { }
fn withReturns(x: String) -> Bool { }

// --- Overloading with complex types ---

fn complexOverload(f: (Int) -> String) { }
fn complexOverload(f: (Float) -> String) { }
fn complexOverload(f: (Int) -> Int) { }
fn complexOverload(f: (Int, Int) -> String) { }

// --- Overloading with tuple types ---

fn tupleOverload(t: (Int, Int)) { }
fn tupleOverload(t: (Int, String)) { }
fn tupleOverload(t: (String, String)) { }
fn tupleOverload(t: (Int, Int, Int)) { }

// --- Visibility doesn't affect overloading ---

public fn visibilityTest(x: Int) { }
private fn visibilityTest(x: Float) { }
internal fn visibilityTest(x: String) { }

// --- Static doesn't affect overloading signature ---

fn staticTest(x: Int) { }
static fn staticTest(x: Float) { }
public static fn staticTest(x: String) { }

// --- Path types as different types ---

fn pathOverload(p: Module.TypeA) { }
fn pathOverload(p: Module.TypeB) { }
fn pathOverload(p: Other.TypeA) { }
