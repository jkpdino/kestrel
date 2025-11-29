module Functions.DuplicateErrors

// ============================================================
// DUPLICATE SIGNATURES - These SHOULD produce errors
// ============================================================

// --- Exact duplicates (same name, same bind name = same external name) ---

func exactDupe(x: Int) { }
func exactDupe(x: Int) { }

// --- Same label, different bind names (label determines external name) ---

func labeledBindDiff(label x: Int) { }
func labeledBindDiff(label y: Int) { }

// --- Multiple parameters, same labels ---

func labelsDupe(from source: String, to dest: String) { }
func labelsDupe(from x: String, to y: String) { }

// --- Return type doesn't distinguish (should be duplicate) ---

func returnTypeDupe(x: Int) -> Int { }
func returnTypeDupe(x: Int) -> String { }

// --- Visibility doesn't distinguish (should be duplicate) ---

public func visibilityDupe(x: Int) { }
private func visibilityDupe(x: Int) { }

// --- Static doesn't distinguish (should be duplicate) ---

func staticDupe(x: Int) { }
static func staticDupe(x: Int) { }

// --- Three or more duplicates ---

func tripleDupe(x: Float) { }
func tripleDupe(x: Float) { }
func tripleDupe(x: Float) { }

// --- Duplicates with complex types ---

func complexDupe(f: (Int, Int) -> Bool) { }
func complexDupe(g: (Int, Int) -> Bool) { }

// --- Duplicates with tuple types ---

func tupleDupe(t: (Int, String)) { }
func tupleDupe(pair: (Int, String)) { }

// --- No parameters duplicate ---

func noParamsDupe() { }
func noParamsDupe() { }
