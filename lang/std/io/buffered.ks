// Buffered I/O - BufferedReader and BufferedWriter
//
// These types wrap readers and writers to provide buffering,
// reducing the number of system calls for small reads/writes.

// Default buffer size (8KB)
let DEFAULT_BUFFER_SIZE: Int = 8192

// BufferedReader - wraps a Reader with a read buffer
public struct BufferedReader[R: Reader, A: Allocator = GlobalAllocator]: Reader {
    private var inner: R
    private var buffer: Array[UInt8, A]
    private var pos: Int      // Current read position in buffer
    private var filled: Int   // Number of valid bytes in buffer

    public init(reader: R) where A == GlobalAllocator {
        self.inner = reader
        self.buffer = Array(capacity: DEFAULT_BUFFER_SIZE)
        self.buffer.resize(to: DEFAULT_BUFFER_SIZE, default: 0)
        self.pos = 0
        self.filled = 0
    }

    public init(reader: R, capacity: Int) where A == GlobalAllocator {
        self.inner = reader
        self.buffer = Array(capacity: capacity)
        self.buffer.resize(to: capacity, default: 0)
        self.pos = 0
        self.filled = 0
    }

    public init(reader: R, allocator: A, capacity: Int) {
        self.inner = reader
        self.buffer = Array(allocator: allocator, capacity: capacity)
        self.buffer.resize(to: capacity, default: 0)
        self.pos = 0
        self.filled = 0
    }

    // Fill the internal buffer
    private func fillBuffer() -> IoResult[Unit] {
        // Move any remaining data to the start
        if self.pos > 0 and self.filled > self.pos {
            let remaining = self.filled - self.pos
            for i in 0..<remaining {
                self.buffer(unchecked: i) = self.buffer(unchecked: self.pos + i)
            }
            self.filled = remaining
            self.pos = 0
        } else {
            self.pos = 0
            self.filled = 0
        }

        // Read more data
        let slice = self.buffer.asSlice().slice(from: self.filled, to: self.buffer.count).unwrap()
        let n = try self.inner.read(buffer: slice)
        self.filled += n
        .Ok(())
    }

    // Get available data in buffer
    private var available: Int {
        self.filled - self.pos
    }

    // Reader implementation
    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        // If request is larger than our buffer, read directly
        if buffer.count >= self.buffer.count {
            // First flush any buffered data
            if self.available > 0 {
                let toRead = if buffer.count < self.available { buffer.count } else { self.available }
                for i in 0..<toRead {
                    buffer(unchecked: i) = self.buffer(unchecked: self.pos + i)
                }
                self.pos += toRead
                return .Ok(toRead)
            }
            // Read directly
            return self.inner.read(buffer: buffer)
        }

        // Fill buffer if empty
        if self.available == 0 {
            try self.fillBuffer()
            if self.available == 0 {
                return .Ok(0)  // EOF
            }
        }

        // Read from buffer
        let toRead = if buffer.count < self.available { buffer.count } else { self.available }
        for i in 0..<toRead {
            buffer(unchecked: i) = self.buffer(unchecked: self.pos + i)
        }
        self.pos += toRead
        .Ok(toRead)
    }

    // Read a line (up to and including newline)
    public func readLine() -> IoResult[Optional[String]] {
        var line: Array[UInt8] = []

        while true {
            // Search for newline in buffer
            var foundNewline = false
            var newlinePos = self.pos

            while newlinePos < self.filled {
                if self.buffer(unchecked: newlinePos) == 10 {  // '\n'
                    foundNewline = true
                    break
                }
                newlinePos += 1
            }

            if foundNewline {
                // Copy up to and including newline
                for i in self.pos..=newlinePos {
                    line.append(self.buffer(unchecked: i))
                }
                self.pos = newlinePos + 1
                return .Ok(.Some(String(utf8: line.asSlice())))
            }

            // Copy everything in buffer and refill
            for i in self.pos..<self.filled {
                line.append(self.buffer(unchecked: i))
            }
            self.pos = self.filled

            try self.fillBuffer()
            if self.available == 0 {
                // EOF
                if line.isEmpty {
                    return .Ok(.None)
                }
                return .Ok(.Some(String(utf8: line.asSlice())))
            }
        }
    }

    // Iterator over lines
    public func lines() -> LinesReader[R, A] {
        LinesReader(reader: self)
    }

    // Peek at buffered data without consuming
    public func peek(buffer: Slice[UInt8]) -> IoResult[Int] {
        if self.available == 0 {
            try self.fillBuffer()
        }

        let toRead = if buffer.count < self.available { buffer.count } else { self.available }
        for i in 0..<toRead {
            buffer(unchecked: i) = self.buffer(unchecked: self.pos + i)
        }
        .Ok(toRead)
    }

    // Consume n bytes without reading them
    public func consume(amount: Int) {
        let toConsume = if amount < self.available { amount } else { self.available }
        self.pos += toConsume
    }

    // Get the inner reader
    public func intoInner() -> R {
        self.inner
    }

    // Get reference to inner reader
    public func getRef() -> ref R {
        self.inner
    }

    public var bufferCapacity: Int {
        self.buffer.count
    }
}

// LinesReader - iterator over lines from BufferedReader
public struct LinesReader[R: Reader, A: Allocator]: Iterator {
    type Item = IoResult[String]

    private var reader: BufferedReader[R, A]

    public init(reader: BufferedReader[R, A]) {
        self.reader = reader
    }

    public func next() -> Optional[IoResult[String]] {
        match self.reader.readLine() {
            .Ok(.Some(let line)) => .Some(.Ok(line)),
            .Ok(.None) => .None,
            .Err(let e) => .Some(.Err(e))
        }
    }
}

// BufferedWriter - wraps a Writer with a write buffer
public struct BufferedWriter[W: Writer, A: Allocator = GlobalAllocator]: Writer {
    private var inner: W
    private var buffer: Array[UInt8, A]
    private var pos: Int  // Current write position in buffer

    public init(writer: W) where A == GlobalAllocator {
        self.inner = writer
        self.buffer = Array(capacity: DEFAULT_BUFFER_SIZE)
        self.buffer.resize(to: DEFAULT_BUFFER_SIZE, default: 0)
        self.pos = 0
    }

    public init(writer: W, capacity: Int) where A == GlobalAllocator {
        self.inner = writer
        self.buffer = Array(capacity: capacity)
        self.buffer.resize(to: capacity, default: 0)
        self.pos = 0
    }

    public init(writer: W, allocator: A, capacity: Int) {
        self.inner = writer
        self.buffer = Array(allocator: allocator, capacity: capacity)
        self.buffer.resize(to: capacity, default: 0)
        self.pos = 0
    }

    // Flush the internal buffer to the inner writer
    private func flushBuffer() -> IoResult[Unit] {
        if self.pos > 0 {
            let slice = self.buffer.asSlice().slice(from: 0, to: self.pos).unwrap()
            try self.inner.writeAll(buffer: slice)
            self.pos = 0
        }
        .Ok(())
    }

    // Available space in buffer
    private var available: Int {
        self.buffer.count - self.pos
    }

    // Writer implementation
    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        // If data is larger than buffer, flush and write directly
        if buffer.count >= self.buffer.count {
            try self.flushBuffer()
            return self.inner.write(buffer: buffer)
        }

        // If data won't fit, flush first
        if buffer.count > self.available {
            try self.flushBuffer()
        }

        // Copy to buffer
        for i in 0..<buffer.count {
            self.buffer(unchecked: self.pos) = buffer(unchecked: i)
            self.pos += 1
        }
        .Ok(buffer.count)
    }

    public func flush() -> IoResult[Unit] {
        try self.flushBuffer()
        self.inner.flush()
    }

    // Get the inner writer (flushes first)
    public func intoInner() -> IoResult[W] {
        try self.flush()
        .Ok(self.inner)
    }

    // Get reference to inner writer
    public func getRef() -> ref W {
        self.inner
    }

    public var bufferCapacity: Int {
        self.buffer.count
    }

    public var bufferedBytes: Int {
        self.pos
    }

    // Destructor - flush on drop
    deinit {
        _ = self.flushBuffer()
    }
}

// LineWriter - flushes after each newline
public struct LineWriter[W: Writer, A: Allocator = GlobalAllocator]: Writer {
    private var inner: BufferedWriter[W, A]

    public init(writer: W) where A == GlobalAllocator {
        self.inner = BufferedWriter(writer: writer)
    }

    public init(writer: W, capacity: Int) where A == GlobalAllocator {
        self.inner = BufferedWriter(writer: writer, capacity: capacity)
    }

    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        let n = try self.inner.write(buffer: buffer)

        // Check if buffer contains newline
        var hasNewline = false
        for i in 0..<buffer.count {
            if buffer(unchecked: i) == 10 {  // '\n'
                hasNewline = true
                break
            }
        }

        if hasNewline {
            try self.inner.flush()
        }
        .Ok(n)
    }

    public func flush() -> IoResult[Unit] {
        self.inner.flush()
    }

    public func intoInner() -> IoResult[W] {
        self.inner.intoInner()
    }
}

// BufReader alias for BufferedReader
public type BufReader[R: Reader] = BufferedReader[R, GlobalAllocator]

// BufWriter alias for BufferedWriter
public type BufWriter[W: Writer] = BufferedWriter[W, GlobalAllocator]
