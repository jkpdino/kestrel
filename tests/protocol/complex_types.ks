module Protocol.ComplexTypes

// Protocols with complex type signatures

// Protocol with nested function types
protocol FunctionComposer {
    func compose(f: (Int) -> String, g: (String) -> Bool) -> (Int) -> Bool
    func curry(f: (Int, Int) -> Int) -> (Int) -> (Int) -> Int
}

// Protocol with deeply nested tuple types
protocol DataProcessor {
    func process(data: ((Int, Int), (String, String))) -> ((Bool, Bool), (Float, Float))
    func merge(a: (Int, (Int, Int)), b: (Int, (Int, Int))) -> (Int, (Int, Int, Int, Int))
}

// Protocol with function returning function
protocol CallbackFactory {
    func createCallback() -> () -> ()
    func createHandler(name: String) -> (Event) -> Bool
    func createTransform() -> (Int) -> (String) -> Bool
}

// Protocol with mixed complex types
protocol ComplexAPI {
    func execute(command: String, args: (String, String), callback: (Result) -> ()) -> Bool
    func query(filter: (Item) -> Bool) -> (Item, Int)
    func transform(input: ((Int) -> String, Int)) -> (String, (String) -> Int)
}

// Protocol with path types in complex positions
protocol PathTypesProtocol {
    func createPoint(x: Int, y: Int) -> Graphics.Point
    func processShape(shape: Graphics.Shapes.Rectangle) -> Graphics.Bounds
    func handleEvent(event: UI.Events.MouseEvent) -> UI.Response
}

// Protocol with unit and never in complex types
protocol SpecialTypes {
    func getVoidHandler() -> () -> ()
    func getPanicHandler() -> (String) -> !
    func wrapUnit(f: () -> ()) -> () -> Bool
}
