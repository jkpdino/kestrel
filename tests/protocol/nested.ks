// Nested protocol scenarios

module Protocols

// Protocol inside a module
protocol ModuleLevelProtocol {
    func moduleMethod()
}

// Public protocol in module
public protocol PublicModuleProtocol {
    func publicModuleMethod() -> String
}

module Protocols.Graphics

// Deeply nested protocol
protocol Renderable {
    func render(to context: Context)
    func bounds() -> (Int, Int, Int, Int)
}

public protocol Animatable {
    func animate(duration: Float)
    func stop()
    func isAnimating() -> Bool
}

module Protocols.Networking

protocol Connection {
    func open()
    func close()
    func isOpen() -> Bool
}

protocol DataSource {
    func fetch(url: String) -> Data
    func post(url: String, data: Data) -> Response
}
