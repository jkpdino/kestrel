// Arithmetic operator protocols

@operator(+)
public protocol Addable[Rhs = Self] {
    type Output
    func add(other: Rhs) -> Output
}

@operator(-)
public protocol Subtractable[Rhs = Self] {
    type Output
    func subtract(other: Rhs) -> Output
}

@operator(*)
public protocol Multipliable[Rhs = Self] {
    type Output
    func multiply(other: Rhs) -> Output
}

@operator(/)
public protocol Divisible[Rhs = Self] {
    type Output
    func divide(other: Rhs) -> Output
}

@operator(%)
public protocol Modulo[Rhs = Self] {
    type Output
    func mod(other: Rhs) -> Output
}

@operator(prefix -)
public protocol Negatable {
    type Output
    func negate() -> Output
}
