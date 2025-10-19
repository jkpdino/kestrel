// ===== Basic Parameter Usage =====

// Single parameter
fn single(x: Int) {}

// Multiple parameters
fn multiple(x: Int, y: String) {}

// Many parameters
fn many(a: Int, b: String, c: Bool, d: Float) {}

// ===== Parameters with Visibility =====

public fn publicWithParams(name: String, age: Int) {}
private fn privateWithParams(id: Int) {}
internal fn internalWithParams(data: String, count: Int) {}

// ===== Functions with Parameters in Classes =====

class Calculator {
  fn add(a: Int, b: Int) {}
  fn subtract(x: Int, y: Int) {}
  fn multiply(m: Int, n: Int) {}
}

public class UserService {
  public fn createUser(name: String, email: String) {}
  private fn validateUser(id: Int) {}
  fn deleteUser(userId: Int) {}
}

// ===== Nested Classes with Parameterized Functions =====

class Database {
  fn query(sql: String) {}

  class Connection {
    fn execute(command: String, params: Int) {}
  }
}

// ===== Edge Cases =====

// Single character parameter names
fn shortParams(x: Int, y: Int, z: Int) {}

// Long parameter names
fn longParams(veryLongParameterName: String, anotherLongParameterName: Int) {}

// Unicode parameter names
fn unicodeParams(café: String, 世界: Int) {}

// Many parameters (stress test)
fn manyParams(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) {}

// ===== Error Cases (commented out) =====

// ERROR: Missing parameter type
// fn noType(x) {}

// ERROR: Missing parameter name
// fn noName(: Int) {}

// ERROR: Trailing comma
// fn trailingComma(x: Int, y: String,) {}

// ERROR: Missing comma between parameters
// fn missingComma(x: Int y: String) {}
