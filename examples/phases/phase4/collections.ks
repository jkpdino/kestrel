// Generic collections and algorithms

module Collections

protocol Comparable[T] {
    func compare(other: T) -> Int
}

struct Pair[K, V] {
    let key: K
    let value: V

    func getKey() -> K {
        self.key
    }

    func getValue() -> V {
        self.value
    }

    func mapValue[U](newValue: U) -> Pair[K, U] {
        Pair[K, U](key: self.key, value: newValue)
    }
}

struct Range {
    let start: Int
    let end: Int

    func length() -> Int {
        if self.end > self.start {
            self.end - self.start
        } else {
            0
        }
    }

    func contains(value: Int) -> Bool {
        value >= self.start and value < self.end
    }

    func overlaps(other: Range) -> Bool {
        self.start < other.end and other.start < self.end
    }

    func intersection(other: Range) -> Range {
        let newStart = if self.start > other.start { self.start } else { other.start };
        let newEnd = if self.end < other.end { self.end } else { other.end };
        Range(start: newStart, end: newEnd)
    }
}

struct Counter {
    var count: Int
    let limit: Int

    init(limit: Int) {
        self.count = 0;
        self.limit = limit;
    }

    mutating func increment() -> Bool {
        if self.count < self.limit {
            self.count = self.count + 1;
            true
        } else {
            false
        }
    }

    mutating func reset() {
        self.count = 0;
    }

    func isAtLimit() -> Bool {
        self.count >= self.limit
    }

    func remaining() -> Int {
        if self.limit > self.count {
            self.limit - self.count
        } else {
            0
        }
    }
}

struct Statistics {
    var sum: Int
    var count: Int
    var min: Int
    var max: Int

    init() {
        self.sum = 0;
        self.count = 0;
        self.min = 0;
        self.max = 0;
    }

    mutating func add(value: Int) {
        if self.count == 0 {
            self.min = value;
            self.max = value;
        } else {
            if value < self.min {
                self.min = value;
            }
            if value > self.max {
                self.max = value;
            }
        }
        self.sum = self.sum + value;
        self.count = self.count + 1;
    }

    func average() -> Int {
        if self.count > 0 {
            self.sum / self.count
        } else {
            0
        }
    }

    func range() -> Int {
        if self.count > 0 {
            self.max - self.min
        } else {
            0
        }
    }
}

func abs(n: Int) -> Int {
    if n < 0 { 0 - n } else { n }
}

func max(a: Int, b: Int) -> Int {
    if a > b { a } else { b }
}

func min(a: Int, b: Int) -> Int {
    if a < b { a } else { b }
}

func clamp(value: Int, low: Int, high: Int) -> Int {
    if value < low {
        low
    } else if value > high {
        high
    } else {
        value
    }
}

func gcd(a: Int, b: Int) -> Int {
    var x = abs(n: a);
    var y = abs(n: b);
    while y != 0 {
        let temp = y;
        y = x % y;
        x = temp;
    }
    x
}

func main() -> Int {
    let pair = Pair[String, Int](key: "count", value: 42);
    let mappedPair = pair.mapValue[Bool](newValue: true);

    let range1 = Range(start: 0, end: 100);
    let range2 = Range(start: 50, end: 150);
    let intersection = range1.intersection(other: range2);

    var counter = Counter(limit: 10);
    var steps = 0;
    while counter.increment() {
        steps = steps + 1;
    }

    var stats = Statistics();
    stats.add(value: 10);
    stats.add(value: 20);
    stats.add(value: 15);
    stats.add(value: 25);
    stats.add(value: 5);

    let avg = stats.average();
    let spread = stats.range();

    let clamped = clamp(value: 150, low: 0, high: 100);
    let divisor = gcd(a: 48, b: 18);

    avg + spread + clamped + divisor
}
