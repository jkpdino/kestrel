module Protocol.Visibility

// Protocol visibility combinations

// Public protocol with methods
public protocol PublicAPI {
    func connect()
    func disconnect()
    func status() -> Bool
}

// Private protocol with methods
private protocol PrivateImpl {
    func internalProcess()
    func cleanup()
}

// Internal protocol with methods
internal protocol InternalProtocol {
    func packageMethod()
    func anotherPackageMethod() -> String
}

// Fileprivate protocol with methods
fileprivate protocol FilePrivateProtocol {
    func localMethod()
    func anotherLocalMethod(x: Int) -> Int
}

// Protocol methods can have their own visibility (for future implementation)
// Currently methods inherit protocol visibility
public protocol MixedVisibility {
    func publicMethod()
    func anotherPublicMethod() -> Int
}
