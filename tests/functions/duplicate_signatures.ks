module Test

// Valid overloads - different signatures
fn add(x: Int) { }
fn add(x: Int, y: Int) { }
fn add(x: Float, y: Float) { }

// Valid overloads - different labels
fn greet(with name: String) { }
fn greet(using name: String) { }

// DUPLICATE - same name, same label, same type
fn duplicate(x: Int) { }
fn duplicate(x: Int) { }

// DUPLICATE - same labels and types (different bind names don't matter)
fn another(label x: String) { }
fn another(label y: String) { }
