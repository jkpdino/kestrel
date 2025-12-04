module Calculator

// ============================================================================
// An Expression Calculator
// ============================================================================
// Demonstrates: all operator types, expression trees, operator precedence,
// and building a calculator-like system

// --- Expression Value Types ---

struct IntValue {
    let value: Int
    
    func add(other: IntValue) -> IntValue {
        IntValue(value: self.value + other.value)
    }
    
    func subtract(other: IntValue) -> IntValue {
        IntValue(value: self.value - other.value)
    }
    
    func multiply(other: IntValue) -> IntValue {
        IntValue(value: self.value * other.value)
    }
    
    func divide(other: IntValue) -> IntValue {
        IntValue(value: self.value / other.value)
    }
    
    func modulo(other: IntValue) -> IntValue {
        IntValue(value: self.value % other.value)
    }
    
    func negate() -> IntValue {
        IntValue(value: -self.value)
    }
    
    func isZero() -> Bool {
        self.value == 0
    }
    
    func isPositive() -> Bool {
        self.value > 0
    }
    
    func isNegative() -> Bool {
        self.value < 0
    }
    
    func equals(other: IntValue) -> Bool {
        self.value == other.value
    }
    
    func lessThan(other: IntValue) -> Bool {
        self.value < other.value
    }
    
    func greaterThan(other: IntValue) -> Bool {
        self.value > other.value
    }
}

struct FloatValue {
    let value: Float
    
    func add(other: FloatValue) -> FloatValue {
        FloatValue(value: self.value + other.value)
    }
    
    func subtract(other: FloatValue) -> FloatValue {
        FloatValue(value: self.value - other.value)
    }
    
    func multiply(other: FloatValue) -> FloatValue {
        FloatValue(value: self.value * other.value)
    }
    
    func divide(other: FloatValue) -> FloatValue {
        FloatValue(value: self.value / other.value)
    }
    
    func negate() -> FloatValue {
        FloatValue(value: -self.value)
    }
    
    func isZero() -> Bool {
        self.value == 0.0
    }
    
    func isPositive() -> Bool {
        self.value > 0.0
    }
    
    func isNegative() -> Bool {
        self.value < 0.0
    }
}

struct BoolValue {
    let value: Bool
    
    func and(other: BoolValue) -> BoolValue {
        BoolValue(value: self.value and other.value)
    }
    
    func or(other: BoolValue) -> BoolValue {
        BoolValue(value: self.value or other.value)
    }
    
    func not() -> BoolValue {
        BoolValue(value: not self.value)
    }
    
    func equals(other: BoolValue) -> Bool {
        self.value == other.value
    }
    
    func implies(other: BoolValue) -> BoolValue {
        BoolValue(value: not self.value or other.value)
    }
    
    func xor(other: BoolValue) -> BoolValue {
        let either: Bool = self.value or other.value
        let notBoth: Bool = not (self.value and other.value)
        BoolValue(value: either and notBoth)
    }
}

// --- Bitwise Calculator ---

struct BitwiseCalc {
    let value: Int
    
    func and(other: BitwiseCalc) -> BitwiseCalc {
        BitwiseCalc(value: self.value & other.value)
    }
    
    func or(other: BitwiseCalc) -> BitwiseCalc {
        BitwiseCalc(value: self.value | other.value)
    }
    
    func xor(other: BitwiseCalc) -> BitwiseCalc {
        BitwiseCalc(value: self.value ^ other.value)
    }
    
    func not() -> BitwiseCalc {
        BitwiseCalc(value: !self.value)
    }
    
    func shiftLeft(bits: Int) -> BitwiseCalc {
        BitwiseCalc(value: self.value << bits)
    }
    
    func shiftRight(bits: Int) -> BitwiseCalc {
        BitwiseCalc(value: self.value >> bits)
    }
    
    func getBit(position: Int) -> Bool {
        let mask: Int = 1 << position
        (self.value & mask) != 0
    }
    
    func setBit(position: Int) -> BitwiseCalc {
        let mask: Int = 1 << position
        BitwiseCalc(value: self.value | mask)
    }
    
    func clearBit(position: Int) -> BitwiseCalc {
        let mask: Int = !(1 << position)
        BitwiseCalc(value: self.value & mask)
    }
    
    func toggleBit(position: Int) -> BitwiseCalc {
        let mask: Int = 1 << position
        BitwiseCalc(value: self.value ^ mask)
    }
}

// --- Accumulator Pattern ---

struct Accumulator {
    var total: Int
    var count: Int
    
    init() {
        self.total = 0
        self.count = 0
    }
    
    init(initial: Int) {
        self.total = initial
        self.count = 1
    }
    
    mutating func add(value: Int) {
        self.total = self.total + value
        self.count = self.count + 1
    }
    
    mutating func subtract(value: Int) {
        self.total = self.total - value
        self.count = self.count + 1
    }
    
    mutating func multiply(value: Int) {
        self.total = self.total * value
        self.count = self.count + 1
    }
    
    mutating func divide(value: Int) {
        self.total = self.total / value
        self.count = self.count + 1
    }
    
    mutating func reset() {
        self.total = 0
        self.count = 0
    }
    
    func average() -> Int {
        self.total / self.count
    }
    
    func isPositive() -> Bool {
        self.total > 0
    }
}

// --- Expression Evaluation Helpers ---

func evaluateAddition(a: Int, b: Int) -> Int {
    a + b
}

func evaluateSubtraction(a: Int, b: Int) -> Int {
    a - b
}

func evaluateMultiplication(a: Int, b: Int) -> Int {
    a * b
}

func evaluateDivision(a: Int, b: Int) -> Int {
    a / b
}

func evaluateModulo(a: Int, b: Int) -> Int {
    a % b
}

func evaluateNegation(a: Int) -> Int {
    -a
}

func evaluateComparison(a: Int, b: Int) -> Bool {
    a == b
}

func evaluateLessThan(a: Int, b: Int) -> Bool {
    a < b
}

func evaluateGreaterThan(a: Int, b: Int) -> Bool {
    a > b
}

// --- Complex Expression Examples ---

// Polynomial: 3x² + 2x + 1
func polynomial(x: Int) -> Int {
    3 * x * x + 2 * x + 1
}

// Distance formula component: (x2 - x1)²
func distanceComponent(x1: Int, x2: Int) -> Int {
    let diff: Int = x2 - x1
    diff * diff
}

// Linear interpolation factor
func lerp(a: Float, b: Float, t: Float) -> Float {
    a + (b - a) * t
}

// Weighted average of two values
func weightedAverage(a: Int, weightA: Int, b: Int, weightB: Int) -> Int {
    (a * weightA + b * weightB) / (weightA + weightB)
}

// Check if value is in exclusive range
func inExclusiveRange(value: Int, min: Int, max: Int) -> Bool {
    value > min and value < max
}

// Check if value is in inclusive range
func inInclusiveRange(value: Int, min: Int, max: Int) -> Bool {
    value >= min and value <= max
}

// Three-way comparison result
func compare(a: Int, b: Int) -> Int {
    1
}

// --- Precedence Demonstrations ---

// Addition and multiplication
func precedenceDemo1() -> Int {
    // 2 + 3 * 4 = 2 + 12 = 14 (not 20)
    2 + 3 * 4
}

// Subtraction and division
func precedenceDemo2() -> Int {
    // 20 - 8 / 2 = 20 - 4 = 16 (not 6)
    20 - 8 / 2
}

// Multiple operators
func precedenceDemo3() -> Int {
    // 1 + 2 * 3 + 4 * 5 = 1 + 6 + 20 = 27
    1 + 2 * 3 + 4 * 5
}

// Parentheses override
func precedenceDemo4() -> Int {
    // (1 + 2) * (3 + 4) = 3 * 7 = 21
    (1 + 2) * (3 + 4)
}

// Comparison with arithmetic
func precedenceDemo5() -> Bool {
    // 2 + 2 == 4 → 4 == 4 → true
    2 + 2 == 4
}

// Logical with comparison
func precedenceDemo6() -> Bool {
    // 1 < 2 and 3 < 4 → true and true → true
    1 < 2 and 3 < 4
}

// Complex mixed expression
func precedenceDemo7() -> Bool {
    // (10 + 5) * 2 > 20 and 100 / 4 == 25
    // 15 * 2 > 20 and 25 == 25
    // 30 > 20 and true
    // true and true → true
    (10 + 5) * 2 > 20 and 100 / 4 == 25
}

// Bitwise with arithmetic
func precedenceDemo8() -> Int {
    // Bitwise has lower precedence than arithmetic in some contexts
    let a: Int = 5 & 3      // 5 AND 3 = 1
    let b: Int = 5 | 3      // 5 OR 3 = 7
    a + b                    // 1 + 7 = 8
}

// --- Calculator Session ---

func runCalculatorSession() {
    // Create values
    let a: IntValue = IntValue(value: 100)
    let b: IntValue = IntValue(value: 25)
    
    // Perform operations
    let sum: IntValue = a.add(b)
    let diff: IntValue = a.subtract(b)
    let prod: IntValue = a.multiply(b)
    let quot: IntValue = a.divide(b)
    
    // Check results
    let sumIs125: Bool = sum.value == 125
    let diffIs75: Bool = diff.value == 75
    let prodIs2500: Bool = prod.value == 2500
    let quotIs4: Bool = quot.value == 4
    
    // Boolean logic
    let t: BoolValue = BoolValue(value: true)
    let f: BoolValue = BoolValue(value: false)
    
    let andResult: BoolValue = t.and(f)
    let orResult: BoolValue = t.or(f)
    let notResult: BoolValue = t.not()
    let xorResult: BoolValue = t.xor(f)
    
    // Bitwise operations
    let bits: BitwiseCalc = BitwiseCalc(value: 0b11110000)
    let shifted: BitwiseCalc = bits.shiftRight(4)
    let masked: BitwiseCalc = bits.and(BitwiseCalc(value: 0b00001111))
    
    // Accumulator
    var acc: Accumulator = Accumulator(initial: 10)
    acc.add(value: 5)
    acc.multiply(value: 2)
    let total: Int = acc.total
    
    ()
}

