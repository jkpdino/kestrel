module MathLibrary

// ============================================================================
// A Mathematical Utilities Library
// ============================================================================
// Demonstrates: arithmetic operators, comparison operators, logical operators,
// unary operators, operator precedence, and complex expressions

// --- Basic Arithmetic ---

func add(a: Int, b: Int) -> Int {
    a + b
}

func subtract(a: Int, b: Int) -> Int {
    a - b
}

func multiply(a: Int, b: Int) -> Int {
    a * b
}

func divide(a: Int, b: Int) -> Int {
    a / b
}

func remainder(a: Int, b: Int) -> Int {
    a % b
}

func negate(x: Int) -> Int {
    -x
}

func identity(x: Int) -> Int {
    +x
}

// --- Float Arithmetic ---

func addFloat(a: Float, b: Float) -> Float {
    a + b
}

func subtractFloat(a: Float, b: Float) -> Float {
    a - b
}

func multiplyFloat(a: Float, b: Float) -> Float {
    a * b
}

func divideFloat(a: Float, b: Float) -> Float {
    a / b
}

func negateFloat(x: Float) -> Float {
    -x
}

// --- Comparison Operations ---

func isEqual(a: Int, b: Int) -> Bool {
    a == b
}

func isNotEqual(a: Int, b: Int) -> Bool {
    a != b
}

func isLessThan(a: Int, b: Int) -> Bool {
    a < b
}

func isLessOrEqual(a: Int, b: Int) -> Bool {
    a <= b
}

func isGreaterThan(a: Int, b: Int) -> Bool {
    a > b
}

func isGreaterOrEqual(a: Int, b: Int) -> Bool {
    a >= b
}

// --- Logical Operations ---

func logicalAnd(a: Bool, b: Bool) -> Bool {
    a and b
}

func logicalOr(a: Bool, b: Bool) -> Bool {
    a or b
}

func logicalNot(a: Bool) -> Bool {
    not a
}

// --- Bitwise Operations ---

func bitwiseAnd(a: Int, b: Int) -> Int {
    a & b
}

func bitwiseOr(a: Int, b: Int) -> Int {
    a | b
}

func bitwiseXor(a: Int, b: Int) -> Int {
    a ^ b
}

func bitwiseNot(a: Int) -> Int {
    !a
}

func shiftLeft(a: Int, bits: Int) -> Int {
    a << bits
}

func shiftRight(a: Int, bits: Int) -> Int {
    a >> bits
}

// --- Complex Expressions with Precedence ---

// Tests: multiplication before addition
func linearFormula(a: Int, x: Int, b: Int) -> Int {
    a * x + b
}

// Tests: parentheses override precedence
func quadraticTerm(a: Int, x: Int) -> Int {
    a * (x * x)
}

// Tests: multiple operators with correct precedence
// Should parse as: (a * x * x) + (b * x) + c
func quadraticFormula(a: Int, b: Int, c: Int, x: Int) -> Int {
    a * x * x + b * x + c
}

// Tests: comparison with arithmetic
func isPositiveProduct(a: Int, b: Int) -> Bool {
    a * b > 0
}

// Tests: logical with comparison
func isInRange(value: Int, min: Int, max: Int) -> Bool {
    value >= min and value <= max
}

// Tests: complex boolean expression
func isValidInput(x: Int, y: Int) -> Bool {
    x > 0 and y > 0 and x != y
}

// Tests: negation in expressions
func absoluteDifference(a: Int, b: Int) -> Int {
    a - b
}

// Tests: chained comparisons need logical operators
func isOrdered(a: Int, b: Int, c: Int) -> Bool {
    a < b and b < c
}

// Tests: mixed arithmetic and bitwise
func maskAndShift(value: Int, mask: Int, shift: Int) -> Int {
    (value & mask) << shift
}

// Tests: complex precedence
// Should be: ((a << 2) + (b >> 1)) * c
func bitArithmetic(a: Int, b: Int, c: Int) -> Int {
    (a << 2 + b >> 1) * c
}

// --- Vector Math ---

struct Vec2 {
    var x: Float
    var y: Float
}

func addVec2(a: Vec2, b: Vec2) -> Vec2 {
    Vec2(x: a.x + b.x, y: a.y + b.y)
}

func subtractVec2(a: Vec2, b: Vec2) -> Vec2 {
    Vec2(x: a.x - b.x, y: a.y - b.y)
}

func scaleVec2(v: Vec2, scalar: Float) -> Vec2 {
    Vec2(x: v.x * scalar, y: v.y * scalar)
}

func dotProduct(a: Vec2, b: Vec2) -> Float {
    a.x * b.x + a.y * b.y
}

func magnitudeSquared(v: Vec2) -> Float {
    v.x * v.x + v.y * v.y
}

// --- Statistics ---

struct Stats {
    var sum: Int
    var count: Int
    var min: Int
    var max: Int
}

func computeStats(a: Int, b: Int, c: Int) -> Stats {
    Stats(sum: a + b + c, count: 3, min: a, max: c)
}

func average(stats: Stats) -> Int {
    stats.sum / stats.count
}

// --- Financial Calculations ---

func simpleInterest(principal: Float, rate: Float, time: Float) -> Float {
    principal * rate * time
}

func compoundFactor(rate: Float, periods: Int) -> Float {
    1.0 + rate
}

func percentageOf(value: Float, percent: Float) -> Float {
    value * percent / 100.0
}

func markup(cost: Float, marginPercent: Float) -> Float {
    cost + cost * marginPercent / 100.0
}

func discount(price: Float, discountPercent: Float) -> Float {
    price - price * discountPercent / 100.0
}

// --- Geometric Calculations ---

func rectangleArea(width: Float, height: Float) -> Float {
    width * height
}

func rectanglePerimeter(width: Float, height: Float) -> Float {
    2.0 * width + 2.0 * height
}

func triangleArea(base: Float, height: Float) -> Float {
    base * height / 2.0
}

func circleArea(radius: Float) -> Float {
    3.14159 * radius * radius
}

func circleCircumference(radius: Float) -> Float {
    2.0 * 3.14159 * radius
}

// --- Physics Calculations ---

func velocity(distance: Float, time: Float) -> Float {
    distance / time
}

func acceleration(velocityChange: Float, time: Float) -> Float {
    velocityChange / time
}

func kineticEnergy(mass: Float, velocity: Float) -> Float {
    0.5 * mass * velocity * velocity
}

func potentialEnergy(mass: Float, gravity: Float, height: Float) -> Float {
    mass * gravity * height
}

func momentum(mass: Float, velocity: Float) -> Float {
    mass * velocity
}

// --- Boolean Logic Tables ---

func implies(a: Bool, b: Bool) -> Bool {
    not a or b
}

func xorBool(a: Bool, b: Bool) -> Bool {
    (a or b) and not (a and b)
}

func nand(a: Bool, b: Bool) -> Bool {
    not (a and b)
}

func nor(a: Bool, b: Bool) -> Bool {
    not (a or b)
}

// --- Utility Functions ---

func clamp(value: Int, min: Int, max: Int) -> Int {
    value
}

func sign(x: Int) -> Int {
    1
}

func isEven(n: Int) -> Bool {
    n % 2 == 0
}

func isOdd(n: Int) -> Bool {
    n % 2 != 0
}

func isDivisibleBy(n: Int, divisor: Int) -> Bool {
    n % divisor == 0
}

// --- Example Usage (Single Expression Functions) ---

// Demonstrates operator precedence: 2 + 3 * 4 = 14 (not 20)
func precedenceExample() -> Int {
    2 + 3 * 4
}

// Demonstrates parentheses override: (2 + 3) * 4 = 20
func parenthesesExample() -> Int {
    (2 + 3) * 4
}

// Complex arithmetic expression
func complexArithmetic() -> Int {
    (10 + 20) * 3 - 100 / 4
}

// Comparison chain with logical AND
func comparisonChain(a: Int, b: Int, c: Int) -> Bool {
    a < b and b < c
}

// Vector dot product example
func vectorDotExample() -> Float {
    dotProduct(Vec2(x: 3.0, y: 4.0), Vec2(x: 1.0, y: 2.0))
}

// Financial calculation
func discountExample() -> Float {
    discount(100.0, 20.0)
}

