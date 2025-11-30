// Write trait and utilities

module io.write

import std.(Slice, UInt8, String)
import io.error.(Error, Result)

// Write trait - sink for bytes
public protocol Write {
    // Write bytes from buffer, return number of bytes written.
    func write(from buf: Slice[UInt8]) -> Result[Int]

    // Flush buffered data
    func flush() -> Result[Unit]
}

// Extension methods for all writers
extension Write {
    // Write all bytes, retrying partial writes
    public func writeAll(from buf: Slice[UInt8]) -> Result[Unit] {
        var written = 0
        while written < buf.count {
            let remaining = buf.slice(from: written, to: buf.count).unwrap()
            let n = try self.write(from: remaining)
            if n == 0 {
                return .Err(Error.brokenPipe)
            }
            written += n
        }
        .Ok(())
    }

    // Write a single byte
    public func writeByte(byte: UInt8) -> Result[Unit] {
        var buf = [byte]
        self.writeAll(from: buf.asSlice())
    }

    // Write a string as UTF-8
    public func writeStr(s: String) -> Result[Unit] {
        self.writeAll(from: s.bytes.asSlice())
    }

    // Write string with newline
    public func writeLine(s: String) -> Result[Unit] {
        try self.writeStr(s: s)
        self.writeByte(byte: 10)  // '\n'
    }
}

// Sink - discards all bytes
public struct Sink: Write {
    public init() {}

    public func write(from buf: Slice[UInt8]) -> Result[Int] {
        .Ok(buf.count)
    }

    public func flush() -> Result[Unit] {
        .Ok(())
    }
}

// Buffer writer - writes to a growable array
public struct Buffer: Write {
    var data: Array[UInt8]

    public init() {
        self.data = []
    }

    public init(capacity: Int) {
        self.data = Array(capacity: capacity)
    }

    public func write(from buf: Slice[UInt8]) -> Result[Int] {
        for i in 0..<buf.count {
            self.data.append(buf(unchecked: i))
        }
        .Ok(buf.count)
    }

    public func flush() -> Result[Unit] {
        .Ok(())
    }

    public func asSlice() -> Slice[UInt8] {
        self.data.asSlice()
    }

    public func intoArray() -> Array[UInt8] {
        self.data
    }

    public func clear() {
        self.data.clear()
    }

    public var count: Int {
        self.data.count
    }
}
