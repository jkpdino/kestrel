module Functions.EdgeCases

// ============================================================
// EDGE CASES - Testing boundary conditions
// ============================================================

// --- Empty parameter list ---

fn emptyParams() { }

// --- Single character names ---

fn a() { }
fn b(x: Int) { }
fn c(a b: Int) { }

// --- Long names ---

fn veryLongFunctionNameThatShouldStillWorkCorrectly() { }
fn anotherLongName(veryLongParameterName: Int) { }
fn withLongLabel(veryLongLabelName paramName: String) { }

// --- All visibility + static combinations ---

fn plain() { }
public fn pub() { }
private fn priv() { }
internal fn intern() { }
fileprivate fn filepriv() { }
static fn stat() { }
public static fn pubStat() { }
private static fn privStat() { }
internal static fn internStat() { }
fileprivate static fn fileprivStat() { }

// --- Label same as bind name ---

fn labelSameAsBind(x x: Int) { }

// --- Label same as function name ---

fn sameName(sameName x: Int) { }

// --- Multiple functions, same labels, different positions ---

fn labelPosition(first a: Int, b: Int) { }
fn labelPosition(a: Int, second b: Int) { }

// --- Unit type in various positions ---

fn unitReturn() -> () { }

// --- Never type ---

fn neverReturn() -> ! { }

// --- Overload with labeled vs unlabeled ---
// The unlabeled uses bind name as external name

fn labeledVsUnlabeled(x: Int) { }
fn labeledVsUnlabeled(label x: Int) { }

// --- Same label, different bind names (should be duplicate) ---

fn sameLabelTest(with a: Int) { }
fn sameLabelTest(with b: Int) { }

// --- Many parameters ---

fn manyParams(a: Int, b: Int, c: Int, d: Int) { }
