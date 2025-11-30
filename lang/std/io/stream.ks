// Stream protocols - Reader, Writer, Seek
//
// These protocols define the core I/O abstractions for reading and writing bytes.

// Reader - trait for reading bytes
public protocol Reader {
    // Read bytes into buffer, returns number of bytes read
    // Returns 0 on EOF
    func read(buffer: Slice[UInt8]) -> IoResult[Int]
}

// Extension methods for Reader
extension Reader {
    // Read exactly n bytes, failing if EOF reached early
    public func readExact(buffer: Slice[UInt8]) -> IoResult[Unit] {
        var offset = 0
        while offset < buffer.count {
            let remaining = buffer.slice(from: offset, to: buffer.count).unwrap()
            let n = try self.read(buffer: remaining)
            if n == 0 {
                return .Err(IoError.unexpectedEof())
            }
            offset += n
        }
        .Ok(())
    }

    // Read all bytes until EOF into an array
    public func readToEnd(buffer: ref Array[UInt8]) -> IoResult[Int] {
        var totalRead = 0
        var chunk: [UInt8; 4096] = [0; 4096]
        let chunkSlice = Slice(pointer: Pointer(to: chunk(unchecked: 0)), count: 4096)

        while true {
            let n = try self.read(buffer: chunkSlice)
            if n == 0 {
                break
            }
            for i in 0..<n {
                buffer.append(chunkSlice(unchecked: i))
            }
            totalRead += n
        }
        .Ok(totalRead)
    }

    // Read a single byte
    public func readByte() -> IoResult[Optional[UInt8]] {
        var buf: [UInt8; 1] = [0]
        let slice = Slice(pointer: Pointer(to: buf(unchecked: 0)), count: 1)
        let n = try self.read(buffer: slice)
        if n == 0 {
            .Ok(.None)
        } else {
            .Ok(.Some(buf(unchecked: 0)))
        }
    }

    // Create a bytes iterator
    public func bytes() -> BytesReader[Self] {
        BytesReader(reader: self)
    }

    // Chain two readers
    public func chain[R: Reader](other: R) -> ChainReader[Self, R] {
        ChainReader(first: self, second: other, readingFirst: true)
    }

    // Take at most n bytes
    public func take(limit: Int) -> TakeReader[Self] {
        TakeReader(reader: self, remaining: limit)
    }
}

// Writer - trait for writing bytes
public protocol Writer {
    // Write bytes from buffer, returns number of bytes written
    func write(buffer: Slice[UInt8]) -> IoResult[Int]

    // Flush any buffered data
    func flush() -> IoResult[Unit]
}

// Extension methods for Writer
extension Writer {
    // Write all bytes, retrying on partial writes
    public func writeAll(buffer: Slice[UInt8]) -> IoResult[Unit] {
        var offset = 0
        while offset < buffer.count {
            let remaining = buffer.slice(from: offset, to: buffer.count).unwrap()
            let n = try self.write(buffer: remaining)
            if n == 0 {
                return .Err(IoError.brokenPipe())
            }
            offset += n
        }
        .Ok(())
    }

    // Write a single byte
    public func writeByte(byte: UInt8) -> IoResult[Unit] {
        var buf: [UInt8; 1] = [byte]
        let slice = Slice(pointer: Pointer(to: buf(unchecked: 0)), count: 1)
        self.writeAll(buffer: slice)
    }

    // Write a string as UTF-8 bytes
    public func writeString(string: String) -> IoResult[Unit] {
        let bytes = string.bytes
        self.writeAll(buffer: bytes.asSlice())
    }

    // Write a line (string + newline)
    public func writeLine(string: String) -> IoResult[Unit] {
        try self.writeString(string: string)
        self.writeByte(byte: 10)  // '\n'
    }
}

// Seek - trait for seeking within a stream
public protocol Seek {
    // Seek to position, returns new position from start
    func seek(pos: SeekFrom) -> IoResult[Int64]
}

// Extension methods for Seek
extension Seek {
    // Rewind to start
    public func rewind() -> IoResult[Unit] {
        _ = try self.seek(pos: .Start(0))
        .Ok(())
    }

    // Get current position
    public func position() -> IoResult[Int64] {
        self.seek(pos: .Current(0))
    }

    // Get stream length (seeks to end and back)
    public func length() -> IoResult[Int64] where Self: Seek {
        let current = try self.position()
        let end = try self.seek(pos: .End(0))
        _ = try self.seek(pos: .Start(current))
        .Ok(end)
    }
}

// ReadSeek - combined Reader + Seek
public protocol ReadSeek: Reader, Seek {}

// WriteSeek - combined Writer + Seek
public protocol WriteSeek: Writer, Seek {}

// ReadWrite - combined Reader + Writer
public protocol ReadWrite: Reader, Writer {}

// BytesReader - iterator over bytes from a Reader
public struct BytesReader[R: Reader]: Iterator {
    type Item = IoResult[UInt8]

    private var reader: R

    public init(reader: R) {
        self.reader = reader
    }

    public func next() -> Optional[IoResult[UInt8]] {
        match self.reader.readByte() {
            .Ok(.Some(let byte)) => .Some(.Ok(byte)),
            .Ok(.None) => .None,
            .Err(let e) => .Some(.Err(e))
        }
    }
}

// ChainReader - chains two readers sequentially
public struct ChainReader[R1: Reader, R2: Reader]: Reader {
    private var first: R1
    private var second: R2
    private var readingFirst: Bool

    public init(first: R1, second: R2, readingFirst: Bool) {
        self.first = first
        self.second = second
        self.readingFirst = readingFirst
    }

    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        if self.readingFirst {
            let n = try self.first.read(buffer: buffer)
            if n == 0 {
                self.readingFirst = false
                return self.second.read(buffer: buffer)
            }
            .Ok(n)
        } else {
            self.second.read(buffer: buffer)
        }
    }
}

// TakeReader - limits reading to n bytes
public struct TakeReader[R: Reader]: Reader {
    private var reader: R
    private var remaining: Int

    public init(reader: R, remaining: Int) {
        self.reader = reader
        self.remaining = remaining
    }

    public var limit: Int {
        self.remaining
    }

    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        if self.remaining == 0 {
            return .Ok(0)
        }

        let maxRead = if buffer.count < self.remaining { buffer.count } else { self.remaining }
        let limitedBuffer = buffer.slice(from: 0, to: maxRead).unwrap()
        let n = try self.reader.read(buffer: limitedBuffer)
        self.remaining -= n
        .Ok(n)
    }
}

// EmptyReader - reader that always returns EOF
public struct EmptyReader: Reader {
    public init() {}

    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        .Ok(0)
    }
}

// RepeatReader - reader that infinitely yields a byte
public struct RepeatReader: Reader {
    private var byte: UInt8

    public init(byte: UInt8) {
        self.byte = byte
    }

    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        for i in 0..<buffer.count {
            buffer(unchecked: i) = self.byte
        }
        .Ok(buffer.count)
    }
}

// SinkWriter - writer that discards all bytes
public struct SinkWriter: Writer {
    public init() {}

    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        .Ok(buffer.count)
    }

    public func flush() -> IoResult[Unit] {
        .Ok(())
    }
}

// Cursor - in-memory reader/writer with seeking
public struct Cursor[A: Allocator = GlobalAllocator]: Reader, Writer, Seek, ReadSeek, WriteSeek, ReadWrite {
    private var buffer: Array[UInt8, A]
    private var pos: Int

    public init() where A == GlobalAllocator {
        self.buffer = []
        self.pos = 0
    }

    public init(data: Array[UInt8, A]) {
        self.buffer = data
        self.pos = 0
    }

    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        let available = self.buffer.count - self.pos
        if available == 0 {
            return .Ok(0)
        }
        let toRead = if buffer.count < available { buffer.count } else { available }
        for i in 0..<toRead {
            buffer(unchecked: i) = self.buffer(unchecked: self.pos + i)
        }
        self.pos += toRead
        .Ok(toRead)
    }

    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        for i in 0..<buffer.count {
            if self.pos < self.buffer.count {
                self.buffer(unchecked: self.pos) = buffer(unchecked: i)
            } else {
                self.buffer.append(buffer(unchecked: i))
            }
            self.pos += 1
        }
        .Ok(buffer.count)
    }

    public func flush() -> IoResult[Unit] {
        .Ok(())
    }

    public func seek(pos: SeekFrom) -> IoResult[Int64] {
        let newPos: Int64 = match pos {
            .Start(let offset) => offset,
            .Current(let offset) => Int64(self.pos) + offset,
            .End(let offset) => Int64(self.buffer.count) + offset
        }
        if newPos < 0 {
            return .Err(IoError.invalidInput(reason: "seek to negative position"))
        }
        self.pos = Int(newPos)
        .Ok(newPos)
    }

    public var position: Int {
        self.pos
    }

    public func intoInner() -> Array[UInt8, A] {
        self.buffer
    }

    public func getRef() -> ref Array[UInt8, A] {
        self.buffer
    }
}

// Copy from reader to writer
public func copy[R: Reader, W: Writer](from reader: ref R, to writer: ref W) -> IoResult[Int64] {
    var totalWritten: Int64 = 0
    var buf: [UInt8; 8192] = [0; 8192]
    let slice = Slice(pointer: Pointer(to: buf(unchecked: 0)), count: 8192)

    while true {
        let n = try reader.read(buffer: slice)
        if n == 0 {
            break
        }
        let toWrite = slice.slice(from: 0, to: n).unwrap()
        try writer.writeAll(buffer: toWrite)
        totalWritten += Int64(n)
    }
    .Ok(totalWritten)
}
