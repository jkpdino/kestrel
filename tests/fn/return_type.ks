// ===== Basic Return Types =====

// Simple return type
func returnsInt() -> Int {}

// Different return types
func returnsString() -> String {}
func returnsBool() -> Bool {}
func returnsFloat() -> Float {}

// ===== Return Types with Parameters =====

func add(x: Int, y: Int) -> Int {}
func concat(a: String, b: String) -> String {}
func compare(x: Int, y: Int) -> Bool {}

// ===== Return Types with Visibility =====

public func publicReturns() -> Int {}
private func privateReturns() -> String {}
internal func internalReturns() -> Bool {}

// ===== Complete Functions (visibility + parameters + return type) =====

public func complete(a: Int, b: String) -> Float {}
private func privateComplete(x: Int) -> Bool {}
internal func internalComplete(name: String, count: Int) -> String {}

// ===== Functions with Return Types in Classes =====

class Calculator {
  func add(a: Int, b: Int) -> Int {}
  func subtract(x: Int, y: Int) -> Int {}
  func isPositive(n: Int) -> Bool {}
}

public class StringUtils {
  public func uppercase(text: String) -> String {}
  private func validate(input: String) -> Bool {}
  func length(s: String) -> Int {}
}

// ===== Nested Classes with Return Types =====

class Service {
  func process(data: String) -> Bool {}

  class Helper {
    func convert(value: Int) -> String {}
    func validate(input: String) -> Bool {}
  }
}

// ===== Real-World Example =====

public class UserRepository {
  public func findUser(id: Int) -> String {}
  public func createUser(name: String, email: String) -> Int {}
  private func validateEmail(email: String) -> Bool {}
  func deleteUser(id: Int) -> Bool {}
}

// ===== Edge Cases =====

// Single character return type (if custom types exist)
func shortReturn() -> Int {}

// Unicode in function names with return types
func café() -> String {}
func 世界() -> Int {}

// Long function with all features
public func veryLongFunctionNameWithParametersAndReturnType(param: String) -> Bool {}

// ===== Error Cases (commented out) =====

// ERROR: Missing return type after arrow
// func noReturnType() -> {}

// ERROR: Arrow without return type
// func justArrow() -> {}

// ERROR: Return type without arrow
// func missingArrow() Int {}

// ERROR: Invalid return type
// func invalidType() -> NotAType {}
