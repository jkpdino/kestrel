// Nil type - represents the absence of a value

public struct Nil: Equatable {
    public init() {}

    public func equals(other: Nil) -> Bool {
        true
    }
}

// The nil constant
public let nil: Nil = Nil()
