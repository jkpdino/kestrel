// ===== Basic Standalone Usage =====

// Simplest function declaration
fn simple() {}

// Multiple functions
fn first() {}
fn second() {}
fn third() {}

// ===== Visibility Modifiers =====

// Default visibility (no modifier)
fn defaultVisibility() {}

// Public function
public fn publicFunction() {}

// Private function
private fn privateFunction() {}

// Internal function
internal fn internalFunction() {}

// Fileprivate function
fileprivate fn fileprivateFunction() {}

// ===== Multiple Features Together =====

class Service {
  fn initialize() {}
  fn process() {}
  fn cleanup() {}
}

public class PublicService {
  public fn publicAPI() {}
  private fn internalHelper() {}
}

// ===== Error Cases (commented out) =====

// ERROR: Missing identifier
// fn () {}

// ERROR: Missing parentheses
// fn noParens {}

// ERROR: Invalid visibility
// invalid fn wrongVisibility() {}

// ERROR: Reserved keyword as name
// fn class() {}

// ERROR: Number as identifier
// fn 123() {}
