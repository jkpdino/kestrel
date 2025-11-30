// Logical operator protocols
// Kestrel uses keyword-style logical operators for clarity

@operator(and)
public protocol And[Rhs = Self] {
    type Output
    func and(other: Rhs) -> Output
}

@operator(or)
public protocol Or[Rhs = Self] {
    type Output
    func or(other: Rhs) -> Output
}

@operator(not)
public protocol Not {
    type Output
    func not() -> Output
}
