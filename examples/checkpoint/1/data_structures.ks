module DataStructures

// A generic container that can hold any type of value
struct Container[T] {
    let value: T
    let id: Int
    let metadata: String

    func getValue() -> T { }
    func getId() -> Int { }

    static func create(value: T, id: Int) -> Container[T] { }
    static func createWithMetadata(value: T, id: Int, meta: String) -> Container[T] { }
}

// A pair of values with different types
struct Pair[A, B] {
    let first: A
    let second: B

    func getFirst() -> A { }
    func getSecond() -> B { }
    func swap() -> Pair[B, A] { }
}

// A record representing a person
struct Person {
    let name: String
    let age: Int
    let email: String
    let isActive: Bool

    func getInfo() -> (String, Int) { }
    func isAdult() -> Bool { }
}

// A record for employee information
struct Employee: Person {
    let employeeId: String
    let department: String
    let salary: Float
}

// A generic list node for linked structures
struct Node[T] {
    let data: T
    let next: Node[T]

    func getData() -> T { }
}

// Protocol for objects that can be compared
protocol Comparable {
    func lessThan(other: Self) -> Bool
    func equals(other: Self) -> Bool
    func greaterThan(other: Self) -> Bool
}

// A sortable pair
struct SortablePair[T]: Comparable {
    let left: T
    let right: T

    func lessThan(other: SortablePair[T]) -> Bool { }
    func equals(other: SortablePair[T]) -> Bool { }
    func greaterThan(other: SortablePair[T]) -> Bool { }
}

// Build various data structures
func createPeople() -> (Person, Person, Person) {
    let alice: Person = ("Alice", 30, "alice@example.com", true)
    let bob: Person = ("Bob", 25, "bob@example.com", false)
    let charlie: Person = ("Charlie", 35, "charlie@example.com", true)

    (alice, bob, charlie)
}

// Create containers of different types
func createContainers() -> (Container[Int], Container[String], Container[Bool]) {
    let intContainer: Container[Int] = (42, 1, "int-container")
    let stringContainer: Container[String] = ("hello", 2, "string-container")
    let boolContainer: Container[Bool] = (true, 3, "bool-container")

    (intContainer, stringContainer, boolContainer)
}

// Create pairs of different type combinations
func createPairs() -> (Pair[Int, String], Pair[Bool, Float], Pair[String, (Int, Int)]) {
    let intStringPair: Pair[Int, String] = (42, "answer")
    let boolFloatPair: Pair[Bool, Float] = (true, 3.14)
    let stringTuplePair: Pair[String, (Int, Int)] = ("coords", (10, 20))

    (intStringPair, boolFloatPair, stringTuplePair)
}
