module Protocol.Methods

// Protocol method declarations

// Protocol with single method
protocol Drawable {
    func draw()
}

// Protocol with method that has return type
protocol Hashable {
    func hash() -> Int
}

// Protocol with multiple methods
protocol Collection {
    func count() -> Int
    func isEmpty() -> Bool
    func clear()
}

// Protocol with method parameters
protocol Comparable {
    func compare(other: Self) -> Int
    func equals(other: Self) -> Bool
}

// Protocol with labeled parameters (Swift-style)
protocol NetworkClient {
    func fetch(from url: String) -> Data
    func post(to url: String, body: Data) -> Response
    func delete(at url: String) -> Bool
}

// Protocol with complex parameter types
protocol Transformer {
    func transform(input: (Int, String)) -> (String, Int)
    func apply(f: (Int) -> String, value: Int) -> String
}

// Protocol with multiple labeled and unlabeled parameters
protocol MessageSender {
    func send(message text: String, to recipient: String, urgent: Bool)
    func broadcast(text: String, to recipients: List)
}

// Protocol with unit return type (explicit)
protocol Lifecycle {
    func initialize() -> ()
    func shutdown() -> ()
}

// Protocol with never return type
protocol ErrorHandler {
    func fatalError(message: String) -> !
}

// Protocol with tuple return type
protocol Coordinate {
    func getPosition() -> (Int, Int)
    func getBounds() -> (Int, Int, Int, Int)
}

// Protocol with function type parameters and returns
protocol EventHandler {
    func onEvent(handler: (Event) -> ())
    func createHandler() -> (Event) -> Bool
}

// Protocol with path type parameters
protocol Geometry {
    func contains(point: Graphics.Point) -> Bool
    func intersects(shape: Graphics.Shape) -> Bool
}
