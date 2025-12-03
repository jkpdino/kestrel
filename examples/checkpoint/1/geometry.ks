module Geometry

// A point in 2D space
struct Point {
    let x: Float
    let y: Float

    func distanceFromOrigin() -> Float { }
    func translate(dx: Float, dy: Float) -> Point { }
    func quadrant() -> Int { }
}

// A line segment connecting two points
struct Line {
    let start: Point
    let end: Point

    func length() -> Float { }
    func midpoint() -> Point { }
}

// A rectangle defined by two opposite corners
struct Rectangle {
    let topLeft: Point
    let bottomRight: Point

    func width() -> Float { }
    func height() -> Float { }
    func area() -> Float { }
    func contains(point: Point) -> Bool { }
}

// A circle in 2D space
struct Circle {
    let center: Point
    let radius: Float

    func area() -> Float { }
    func circumference() -> Float { }
    func intersectsWith(other: Circle) -> Bool { }
}

// Protocol for shapes that can be drawn
protocol Drawable {
    func getArea() -> Float
    func getPerimeter() -> Float
}

// Implement drawing protocol for Circle
struct DrawableCircle: Drawable {
    let circle: Circle

    func getArea() -> Float { }
    func getPerimeter() -> Float { }
}

// Create some sample geometric data
func createSampleScene() -> (Point, Line, Circle, Rectangle) {
    let origin: Point = (0.0, 0.0)
    let endPoint: Point = (10.0, 10.0)
    let sampleLine: Line = (origin, endPoint)

    let circle: Circle = ((5.0, 5.0), 7.5)
    let rect: Rectangle = ((0.0, 10.0), (10.0, 0.0))

    (origin, sampleLine, circle, rect)
}
