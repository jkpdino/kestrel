module Functions.DuplicateErrors

// ============================================================
// DUPLICATE SIGNATURES - These SHOULD produce errors
// ============================================================

// --- Exact duplicates (same name, same bind name = same external name) ---

fn exactDupe(x: Int) { }
fn exactDupe(x: Int) { }

// --- Same label, different bind names (label determines external name) ---

fn labeledBindDiff(label x: Int) { }
fn labeledBindDiff(label y: Int) { }

// --- Multiple parameters, same labels ---

fn labelsDupe(from source: String, to dest: String) { }
fn labelsDupe(from x: String, to y: String) { }

// --- Return type doesn't distinguish (should be duplicate) ---

fn returnTypeDupe(x: Int) -> Int { }
fn returnTypeDupe(x: Int) -> String { }

// --- Visibility doesn't distinguish (should be duplicate) ---

public fn visibilityDupe(x: Int) { }
private fn visibilityDupe(x: Int) { }

// --- Static doesn't distinguish (should be duplicate) ---

fn staticDupe(x: Int) { }
static fn staticDupe(x: Int) { }

// --- Three or more duplicates ---

fn tripleDupe(x: Float) { }
fn tripleDupe(x: Float) { }
fn tripleDupe(x: Float) { }

// --- Duplicates with complex types ---

fn complexDupe(f: (Int, Int) -> Bool) { }
fn complexDupe(g: (Int, Int) -> Bool) { }

// --- Duplicates with tuple types ---

fn tupleDupe(t: (Int, String)) { }
fn tupleDupe(pair: (Int, String)) { }

// --- No parameters duplicate ---

fn noParamsDupe() { }
fn noParamsDupe() { }
