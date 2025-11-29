module Functions.EdgeCases

// ============================================================
// EDGE CASES - Testing boundary conditions
// ============================================================

// --- Empty parameter list ---

func emptyParams() { }

// --- Single character names ---

func a() { }
func b(x: Int) { }
func c(a b: Int) { }

// --- Long names ---

func veryLongFunctionNameThatShouldStillWorkCorrectly() { }
func anotherLongName(veryLongParameterName: Int) { }
func withLongLabel(veryLongLabelName paramName: String) { }

// --- All visibility + static combinations ---

func plain() { }
public func pub() { }
private func priv() { }
internal func intern() { }
fileprivate func filepriv() { }
static func stat() { }
public static func pubStat() { }
private static func privStat() { }
internal static func internStat() { }
fileprivate static func fileprivStat() { }

// --- Label same as bind name ---

func labelSameAsBind(x x: Int) { }

// --- Label same as function name ---

func sameName(sameName x: Int) { }

// --- Multiple functions, same labels, different positions ---

func labelPosition(first a: Int, b: Int) { }
func labelPosition(a: Int, second b: Int) { }

// --- Unit type in various positions ---

func unitReturn() -> () { }

// --- Never type ---

func neverReturn() -> ! { }

// --- Overload with labeled vs unlabeled ---
// The unlabeled uses bind name as external name

func labeledVsUnlabeled(x: Int) { }
func labeledVsUnlabeled(label x: Int) { }

// --- Same label, different bind names (should be duplicate) ---

func sameLabelTest(with a: Int) { }
func sameLabelTest(with b: Int) { }

// --- Many parameters ---

func manyParams(a: Int, b: Int, c: Int, d: Int) { }
