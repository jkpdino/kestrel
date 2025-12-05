// A simple expression calculator with operation history

module Calculator

struct Operation {
    let left: Int
    let right: Int
    let result: Int
}

struct Calculator {
    var lastResult: Int
    var operationCount: Int

    init() {
        self.lastResult = 0;
        self.operationCount = 0;
    }

    mutating func add(a: Int, b: Int) -> Int {
        let result = a + b;
        self.lastResult = result;
        self.operationCount = self.operationCount + 1;
        result
    }

    mutating func subtract(a: Int, b: Int) -> Int {
        let result = a - b;
        self.lastResult = result;
        self.operationCount = self.operationCount + 1;
        result
    }

    mutating func multiply(a: Int, b: Int) -> Int {
        let result = a * b;
        self.lastResult = result;
        self.operationCount = self.operationCount + 1;
        result
    }

    func getLastResult() -> Int {
        self.lastResult
    }

    func getOperationCount() -> Int {
        self.operationCount
    }
}

func factorial(n: Int) -> Int {
    var result = 1;
    var i = 2;
    while i <= n {
        result = result * i;
        i = i + 1;
    }
    result
}

func fibonacci(n: Int) -> Int {
    if n <= 1 {
        n
    } else {
        var a = 0;
        var b = 1;
        var i = 2;
        while i <= n {
            let temp = a + b;
            a = b;
            b = temp;
            i = i + 1;
        }
        b
    }
}

func isPrime(n: Int) -> Bool {
    if n < 2 {
        return false;
    }
    var i = 2;
    while i * i <= n {
        if n % i == 0 {
            return false;
        }
        i = i + 1;
    }
    true
}

func main() -> Int {
    var calc = Calculator();

    let sum = calc.add(a: 15, b: 27);
    let diff = calc.subtract(a: 100, b: 37);
    let prod = calc.multiply(a: 6, b: 7);

    let fact5 = factorial(n: 5);
    let fib10 = fibonacci(n: 10);

    calc.getOperationCount()
}
