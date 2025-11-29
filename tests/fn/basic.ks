// ===== Basic Standalone Usage =====

// Simplest function declaration
func simple() {}

// Multiple functions
func first() {}
func second() {}
func third() {}

// ===== Visibility Modifiers =====

// Default visibility (no modifier)
func defaultVisibility() {}

// Public function
public func publicFunction() {}

// Private function
private func privateFunction() {}

// Internal function
internal func internalFunction() {}

// Fileprivate function
fileprivate func fileprivateFunction() {}

// ===== Multiple Features Together =====

class Service {
  func initialize() {}
  func process() {}
  func cleanup() {}
}

public class PublicService {
  public func publicAPI() {}
  private func internalHelper() {}
}

// ===== Error Cases (commented out) =====

// ERROR: Missing identifier
// func () {}

// ERROR: Missing parentheses
// func noParens {}

// ERROR: Invalid visibility
// invalid func wrongVisibility() {}

// ERROR: Reserved keyword as name
// func class() {}

// ERROR: Number as identifier
// func 123() {}
