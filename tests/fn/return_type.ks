// ===== Basic Return Types =====

// Simple return type
fn returnsInt() -> Int {}

// Different return types
fn returnsString() -> String {}
fn returnsBool() -> Bool {}
fn returnsFloat() -> Float {}

// ===== Return Types with Parameters =====

fn add(x: Int, y: Int) -> Int {}
fn concat(a: String, b: String) -> String {}
fn compare(x: Int, y: Int) -> Bool {}

// ===== Return Types with Visibility =====

public fn publicReturns() -> Int {}
private fn privateReturns() -> String {}
internal fn internalReturns() -> Bool {}

// ===== Complete Functions (visibility + parameters + return type) =====

public fn complete(a: Int, b: String) -> Float {}
private fn privateComplete(x: Int) -> Bool {}
internal fn internalComplete(name: String, count: Int) -> String {}

// ===== Functions with Return Types in Classes =====

class Calculator {
  fn add(a: Int, b: Int) -> Int {}
  fn subtract(x: Int, y: Int) -> Int {}
  fn isPositive(n: Int) -> Bool {}
}

public class StringUtils {
  public fn uppercase(text: String) -> String {}
  private fn validate(input: String) -> Bool {}
  fn length(s: String) -> Int {}
}

// ===== Nested Classes with Return Types =====

class Service {
  fn process(data: String) -> Bool {}

  class Helper {
    fn convert(value: Int) -> String {}
    fn validate(input: String) -> Bool {}
  }
}

// ===== Real-World Example =====

public class UserRepository {
  public fn findUser(id: Int) -> String {}
  public fn createUser(name: String, email: String) -> Int {}
  private fn validateEmail(email: String) -> Bool {}
  fn deleteUser(id: Int) -> Bool {}
}

// ===== Edge Cases =====

// Single character return type (if custom types exist)
fn shortReturn() -> Int {}

// Unicode in function names with return types
fn café() -> String {}
fn 世界() -> Int {}

// Long function with all features
public fn veryLongFunctionNameWithParametersAndReturnType(param: String) -> Bool {}

// ===== Error Cases (commented out) =====

// ERROR: Missing return type after arrow
// fn noReturnType() -> {}

// ERROR: Arrow without return type
// fn justArrow() -> {}

// ERROR: Return type without arrow
// fn missingArrow() Int {}

// ERROR: Invalid return type
// fn invalidType() -> NotAType {}
