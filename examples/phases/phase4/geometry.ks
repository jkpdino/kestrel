// 2D Geometry library with shapes and transformations

module Geometry

struct Point {
    let x: Int
    let y: Int

    func distanceSquared(other: Point) -> Int {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        dx * dx + dy * dy
    }

    func translate(dx: Int, dy: Int) -> Point {
        Point(x: self.x + dx, y: self.y + dy)
    }

    func scale(factor: Int) -> Point {
        Point(x: self.x * factor, y: self.y * factor)
    }
}

struct Rectangle {
    let origin: Point
    let width: Int
    let height: Int

    func area() -> Int {
        self.width * self.height
    }

    func perimeter() -> Int {
        2 * (self.width + self.height)
    }

    func contains(point: Point) -> Bool {
        let inX = point.x >= self.origin.x and point.x <= self.origin.x + self.width;
        let inY = point.y >= self.origin.y and point.y <= self.origin.y + self.height;
        inX and inY
    }

    func topRight() -> Point {
        Point(x: self.origin.x + self.width, y: self.origin.y + self.height)
    }

    func center() -> Point {
        Point(
            x: self.origin.x + self.width / 2,
            y: self.origin.y + self.height / 2
        )
    }
}

struct Circle {
    let center: Point
    let radius: Int

    func diameterSquared() -> Int {
        4 * self.radius * self.radius
    }

    func containsPoint(point: Point) -> Bool {
        let distSq = self.center.distanceSquared(other: point);
        distSq <= self.radius * self.radius
    }

    func translate(dx: Int, dy: Int) -> Circle {
        Circle(
            center: self.center.translate(dx: dx, dy: dy),
            radius: self.radius
        )
    }
}

struct BoundingBox {
    let minPoint: Point
    let maxPoint: Point

    func width() -> Int {
        self.maxPoint.x - self.minPoint.x
    }

    func height() -> Int {
        self.maxPoint.y - self.minPoint.y
    }

    func intersects(other: BoundingBox) -> Bool {
        let noOverlapX = self.maxPoint.x < other.minPoint.x or other.maxPoint.x < self.minPoint.x;
        let noOverlapY = self.maxPoint.y < other.minPoint.y or other.maxPoint.y < self.minPoint.y;
        not noOverlapX and not noOverlapY
    }
}

func makeRectangle(x: Int, y: Int, w: Int, h: Int) -> Rectangle {
    Rectangle(origin: Point(x: x, y: y), width: w, height: h)
}

func rectangleToBoundingBox(rect: Rectangle) -> BoundingBox {
    BoundingBox(
        minPoint: rect.origin,
        maxPoint: rect.topRight()
    )
}

func main() -> Int {
    let origin = Point(x: 0, y: 0);
    let rect = makeRectangle(x: 10, y: 10, w: 100, h: 50);

    let area = rect.area();
    let perim = rect.perimeter();

    let testPoint = Point(x: 50, y: 30);
    let isInside = rect.contains(point: testPoint);

    let circle = Circle(center: origin, radius: 25);
    let movedCircle = circle.translate(dx: 100, dy: 100);

    let box1 = rectangleToBoundingBox(rect: rect);
    let box2 = BoundingBox(
        minPoint: Point(x: 50, y: 25),
        maxPoint: Point(x: 150, y: 75)
    );

    if box1.intersects(other: box2) {
        area
    } else {
        0
    }
}
