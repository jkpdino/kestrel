module Main

struct Pair[T, U] { }

protocol Transform[T] {
    func apply(value: T) -> T
}

struct Container[T] {
    static func wrap[U](item: U) -> [U] { [item] }
}

func example() -> Int {
    let x: Int = 42;
    let y: Int = 10;
    let msg: String = "hello";
    x
}

func identity[T](x: T) -> T { x }
func compose() -> (Int, (String, Bool)) { (42, ("hello", true)) }
func matrix() -> [[Int]] { [[1, 2], [3, 4]] }
