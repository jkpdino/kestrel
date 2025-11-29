module Test

// Valid overloads - different signatures
func add(x: Int) { }
func add(x: Int, y: Int) { }
func add(x: Float, y: Float) { }

// Valid overloads - different labels
func greet(with name: String) { }
func greet(using name: String) { }

// DUPLICATE - same name, same label, same type
func duplicate(x: Int) { }
func duplicate(x: Int) { }

// DUPLICATE - same labels and types (different bind names don't matter)
func another(label x: String) { }
func another(label y: String) { }
