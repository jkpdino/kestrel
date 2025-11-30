// Literal protocols - types that can be constructed from literals

@literal
public protocol ExpressibleByBoolLiteral {
    init(boolLiteral value: Bool)
}

@literal
public protocol ExpressibleByIntLiteral {
    init(intLiteral value: Int)
}

@literal
public protocol ExpressibleByFloatLiteral {
    init(floatLiteral value: Float64)
}

@literal
public protocol ExpressibleByStringLiteral {
    init(stringLiteral value: String)
}

@literal
public protocol ExpressibleByNilLiteral {
    init(nilLiteral value: Nil)
}

@literal
public protocol ExpressibleByArrayLiteral {
    type Element
    init(arrayLiteral elements: [Element])
}

@literal
public protocol ExpressibleByDictionaryLiteral {
    type Key
    type Value
    init(dictionaryLiteral pairs: [(Key, Value)])
}
