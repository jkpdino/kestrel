module Protocol.EdgeCases

// Edge cases and special scenarios for protocols

// Empty protocol (marker protocol)
protocol Marker { }

// Protocol with single no-arg method
protocol Initializable {
    func init()
}

// Protocol with single method returning unit
protocol Runnable {
    func run() -> ()
}

// Protocol with many methods
protocol LargeProtocol {
    func method1()
    func method2()
    func method3()
    func method4()
    func method5()
    func method6()
    func method7()
    func method8()
    func method9()
    func method10()
}

// Protocol with long method names
protocol VerboseProtocol {
    func thisIsAVeryLongMethodNameThatTestsParserLimits()
    func anotherExtremelyLongMethodNameForTesting() -> Bool
}

// Protocol with single character names
protocol A {
    func x()
    func y() -> Int
    func z(a: Int) -> Bool
}

// Protocol with underscore in names
protocol Under_Score_Protocol {
    func method_with_underscores()
    func another_method_here() -> Some_Type
}

// Protocol with numbers in names
protocol Protocol123 {
    func method1()
    func method2Returns() -> Type2
}

// Multiple parameters with same type
protocol SameTypes {
    func allInts(a: Int, b: Int, c: Int, d: Int) -> Int
    func allStrings(x: String, y: String, z: String) -> String
}

// Protocol method with many parameters
protocol ManyParams {
    func manyParameters(a: Int, b: String, c: Float, d: Bool, e: Int, f: String) -> (Int, String)
}

// Protocols that might be used together
protocol Readable {
    func read() -> Data
}

protocol Writable {
    func write(data: Data)
}

protocol ReadWrite {
    func read() -> Data
    func write(data: Data)
    func flush()
}
