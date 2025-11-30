// Standard I/O - stdin, stdout, stderr
//
// Provides access to standard input, output, and error streams.

// Stdin - standard input stream
public struct Stdin: Reader {
    private var fd: BorrowedFd

    private init() {
        self.fd = BorrowedFd(fd: RawFd.stdin)
    }

    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        self.fd.read(buffer: buffer)
    }

    // Lock stdin for exclusive buffered access
    public func lock() -> StdinLock {
        StdinLock(reader: BufferedReader(reader: self))
    }

    // Read a line from stdin
    public func readLine() -> IoResult[String] {
        var lock = self.lock()
        match try lock.readLine() {
            .Some(let line) => .Ok(line),
            .None => .Ok("")
        }
    }
}

// StdinLock - locked, buffered stdin
public struct StdinLock: Reader {
    private var reader: BufferedReader[Stdin]

    fileprivate init(reader: BufferedReader[Stdin]) {
        self.reader = reader
    }

    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        self.reader.read(buffer: buffer)
    }

    public func readLine() -> IoResult[Optional[String]] {
        self.reader.readLine()
    }

    public func lines() -> LinesReader[Stdin, GlobalAllocator] {
        self.reader.lines()
    }
}

// Stdout - standard output stream
public struct Stdout: Writer {
    private var fd: BorrowedFd

    private init() {
        self.fd = BorrowedFd(fd: RawFd.stdout)
    }

    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        self.fd.write(buffer: buffer)
    }

    public func flush() -> IoResult[Unit] {
        // stdout is unbuffered at this level
        .Ok(())
    }

    // Lock stdout for exclusive buffered access
    public func lock() -> StdoutLock {
        StdoutLock(writer: LineWriter(writer: self))
    }
}

// StdoutLock - locked, line-buffered stdout
public struct StdoutLock: Writer {
    private var writer: LineWriter[Stdout]

    fileprivate init(writer: LineWriter[Stdout]) {
        self.writer = writer
    }

    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        self.writer.write(buffer: buffer)
    }

    public func flush() -> IoResult[Unit] {
        self.writer.flush()
    }
}

// Stderr - standard error stream
public struct Stderr: Writer {
    private var fd: BorrowedFd

    private init() {
        self.fd = BorrowedFd(fd: RawFd.stderr)
    }

    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        self.fd.write(buffer: buffer)
    }

    public func flush() -> IoResult[Unit] {
        // stderr is unbuffered
        .Ok(())
    }

    // Lock stderr for exclusive access
    public func lock() -> StderrLock {
        StderrLock(writer: self)
    }
}

// StderrLock - locked stderr (unbuffered)
public struct StderrLock: Writer {
    private var writer: Stderr

    fileprivate init(writer: Stderr) {
        self.writer = writer
    }

    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        self.writer.write(buffer: buffer)
    }

    public func flush() -> IoResult[Unit] {
        self.writer.flush()
    }
}

// Global accessor functions

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

// Convenience functions for common I/O operations

// Print a string to stdout (no newline)
public func print(message: String) -> IoResult[Unit] {
    var out = stdout()
    out.writeString(string: message)
}

// Print a string to stdout with newline
public func println(message: String) -> IoResult[Unit] {
    var out = stdout()
    out.writeLine(string: message)
}

// Print to stdout (empty line)
public func println() -> IoResult[Unit] {
    var out = stdout()
    out.writeByte(byte: 10)
}

// Print a string to stderr (no newline)
public func eprint(message: String) -> IoResult[Unit] {
    var err = stderr()
    err.writeString(string: message)
}

// Print a string to stderr with newline
public func eprintln(message: String) -> IoResult[Unit] {
    var err = stderr()
    err.writeLine(string: message)
}

// Read a line from stdin (strips trailing newline)
public func readLine() -> IoResult[String] {
    var line = try stdin().readLine()
    // Strip trailing newline if present
    if line.ends(with: "\n") {
        line = line.substringBytes(from: 0, to: line.byteCount - 1)
    }
    // Also strip \r if present (Windows line endings)
    if line.ends(with: "\r") {
        line = line.substringBytes(from: 0, to: line.byteCount - 1)
    }
    .Ok(line)
}

// Read a line and parse as Int
public func readInt() -> IoResult[Optional[Int]] {
    let line = try readLine()
    .Ok(Int.parse(string: line))
}

// Read a line and parse as Float
public func readFloat() -> IoResult[Optional[Float]] {
    let line = try readLine()
    .Ok(Float.parse(string: line))
}

// Prompt user and read input
public func prompt(message: String) -> IoResult[String] {
    try print(message: message)
    try stdout().flush()
    readLine()
}
