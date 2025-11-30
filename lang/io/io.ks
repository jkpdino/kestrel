// Kestrel I/O Library
//
// A simple I/O library built on libc.
//
// Required compiler intrinsics (map directly to libc):
//   lang.libc_open(path, flags, mode) -> Int32
//   lang.libc_close(fd) -> Int32
//   lang.libc_read(fd, buf, count) -> Int
//   lang.libc_write(fd, buf, count) -> Int
//   lang.libc_lseek(fd, offset, whence) -> Int64
//   lang.libc_errno() -> Int32
//
// Example usage:
//
//   import io
//
//   // Read a file
//   let content = try io.readString(path: "hello.txt")
//
//   // Write a file
//   try io.writeString(path: "out.txt", content: "Hello!")
//
//   // Use File directly
//   var file = try io.File.open(path: "data.txt")
//   var buf = Array[UInt8](capacity: 1024)
//   buf.resize(to: 1024, default: 0)
//   let n = try file.read(into: buf.asSlice())
//
//   // Standard I/O
//   let name = try io.prompt(message: "Name: ")
//   try io.println(s: "Hello, " + name)

module io

// Low-level libc bindings
public import io.libc

// Error types
public import io.error.(Error, Result)

// Read trait and utilities
public import io.read.(
    Read,
    Bytes,
    Take,
    Chain,
    Empty,
    Repeat,
    Cursor
)

// Write trait and utilities
public import io.write.(
    Write,
    Sink,
    Buffer
)

// File I/O
public import io.file.(
    Seek,
    File,
    readString,
    readBytes,
    writeString,
    writeBytes,
    appendString
)

// Standard I/O
public import io.stdio.(
    Stdin,
    Stdout,
    Stderr,
    stdin,
    stdout,
    stderr,
    print,
    println,
    eprint,
    eprintln,
    readLine,
    prompt
)
