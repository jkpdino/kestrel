module Main

struct Point {
    let x: Int
    let y: Int
}

struct Line {
    let start: Point
    let end: Point
}

func getX(p: Point) -> Int {
    p.x
}

func getStartX(line: Line) -> Int {
    line.start.x
}
