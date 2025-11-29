module Protocol.ErrorCases

// Error cases for protocols
// These should parse but produce semantic errors

// Protocol method with body (should error - protocols only declare signatures)
// TODO: This should produce a semantic error "Protocol methods cannot have bodies"
protocol BadProtocol {
    func methodWithBody() { }
}

// Another protocol with method body
protocol AnotherBadProtocol {
    func validMethod()
    func invalidMethod() { }
    func anotherValidMethod() -> Int
}

// Protocol method with body and parameters
protocol ProtocolWithBodyAndParams {
    func process(x: Int, y: String) { }
}

// Protocol method with body and return type
protocol ProtocolWithBodyAndReturn {
    func compute() -> Int { }
}
