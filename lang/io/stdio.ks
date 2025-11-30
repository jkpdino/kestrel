// Standard I/O streams

module io.stdio

import std.(Optional, Array, Slice, UInt8, String)
import io.libc
import io.error.(Error, Result)
import io.read.(Read)
import io.write.(Write)

// Stdin - standard input
public struct Stdin: Read {
    public init() {}

    public func read(into buf: Slice[UInt8]) -> Result[Int] {
        let n = libc.read(libc.STDIN, buf.pointer, UInt(buf.count))
        if n < 0 {
            return .Err(Error.last())
        }
        .Ok(n)
    }
}

// Stdout - standard output
public struct Stdout: Write {
    public init() {}

    public func write(from buf: Slice[UInt8]) -> Result[Int] {
        let n = libc.write(libc.STDOUT, buf.pointer, UInt(buf.count))
        if n < 0 {
            return .Err(Error.last())
        }
        .Ok(n)
    }

    public func flush() -> Result[Unit] {
        .Ok(())
    }
}

// Stderr - standard error
public struct Stderr: Write {
    public init() {}

    public func write(from buf: Slice[UInt8]) -> Result[Int] {
        let n = libc.write(libc.STDERR, buf.pointer, UInt(buf.count))
        if n < 0 {
            return .Err(Error.last())
        }
        .Ok(n)
    }

    public func flush() -> Result[Unit] {
        .Ok(())
    }
}

// Get stdin handle
public func stdin() -> Stdin {
    Stdin()
}

// Get stdout handle
public func stdout() -> Stdout {
    Stdout()
}

// Get stderr handle
public func stderr() -> Stderr {
    Stderr()
}

// Print string to stdout (no newline)
public func print(s: String) -> Result[Unit] {
    var out = stdout()
    out.writeStr(s: s)
}

// Print string to stdout with newline
public func println(s: String) -> Result[Unit] {
    var out = stdout()
    out.writeLine(s: s)
}

// Print empty line
public func println() -> Result[Unit] {
    var out = stdout()
    out.writeByte(byte: 10)
}

// Print to stderr (no newline)
public func eprint(s: String) -> Result[Unit] {
    var err = stderr()
    err.writeStr(s: s)
}

// Print to stderr with newline
public func eprintln(s: String) -> Result[Unit] {
    var err = stderr()
    err.writeLine(s: s)
}

// Read line from stdin
public func readLine() -> Result[String] {
    var input = stdin()
    var bytes: Array[UInt8] = []

    while true {
        match try input.readByte() {
            .Some(let b) => {
                if b == 10 { break }  // newline
                bytes.append(b)
            },
            .None => break  // EOF
        }
    }

    // Strip trailing \r if present (Windows line endings)
    if bytes.count > 0 and bytes(unchecked: bytes.count - 1) == 13 {
        _ = bytes.pop()
    }

    .Ok(String(utf8: bytes.asSlice()))
}

// Prompt and read line
public func prompt(message: String) -> Result[String] {
    try print(s: message)
    try stdout().flush()
    readLine()
}
