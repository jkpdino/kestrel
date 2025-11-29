// ===== Basic Parameter Usage =====

// Single parameter
func single(x: Int) {}

// Multiple parameters
func multiple(x: Int, y: String) {}

// Many parameters
func many(a: Int, b: String, c: Bool, d: Float) {}

// ===== Parameters with Visibility =====

public func publicWithParams(name: String, age: Int) {}
private func privateWithParams(id: Int) {}
internal func internalWithParams(data: String, count: Int) {}

// ===== Functions with Parameters in Classes =====

class Calculator {
  func add(a: Int, b: Int) {}
  func subtract(x: Int, y: Int) {}
  func multiply(m: Int, n: Int) {}
}

public class UserService {
  public func createUser(name: String, email: String) {}
  private func validateUser(id: Int) {}
  func deleteUser(userId: Int) {}
}

// ===== Nested Classes with Parameterized Functions =====

class Database {
  func query(sql: String) {}

  class Connection {
    func execute(command: String, params: Int) {}
  }
}

// ===== Edge Cases =====

// Single character parameter names
func shortParams(x: Int, y: Int, z: Int) {}

// Long parameter names
func longParams(veryLongParameterName: String, anotherLongParameterName: Int) {}

// Unicode parameter names
func unicodeParams(café: String, 世界: Int) {}

// Many parameters (stress test)
func manyParams(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) {}

// ===== Error Cases (commented out) =====

// ERROR: Missing parameter type
// func noType(x) {}

// ERROR: Missing parameter name
// func noName(: Int) {}

// ERROR: Trailing comma
// func trailingComma(x: Int, y: String,) {}

// ERROR: Missing comma between parameters
// func missingComma(x: Int y: String) {}
