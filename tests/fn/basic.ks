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

// ===== Functions in Classes =====

class Container {
  fn method() {}
  fn anotherMethod() {}
}

class WithVisibility {
  public fn publicMethod() {}
  private fn privateMethod() {}
  internal fn internalMethod() {}
}

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

// ===== Nested Classes with Functions =====

class Outer {
  fn outerMethod() {}

  class Inner {
    fn innerMethod() {}
  }
}

// ===== Edge Cases =====

// Single character name
fn x() {}

// Long identifier
fn thisIsAVeryLongFunctionNameThatIsStillValidButUnusual() {}

// Unicode identifiers
fn café() {}
fn 世界() {}
fn привет() {}
fn αβγ() {}

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
