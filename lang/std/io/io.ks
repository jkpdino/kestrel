// Kestrel I/O Module
//
// This module provides I/O primitives built on libc.
//
// The following lang intrinsics are required for this module:
//
// File operations (libc wrappers):
//   lang.io_open(path: lang.ptr[lang.u8], flags: Int32, mode: Int32) -> Int32
//   lang.io_close(fd: Int32) -> Int32
//   lang.io_read(fd: Int32, buf: lang.ptr[lang.u8], count: Int) -> Int
//   lang.io_write(fd: Int32, buf: lang.ptr[lang.u8], count: Int) -> Int
//   lang.io_seek(fd: Int32, offset: Int64, whence: Int32) -> Int64
//   lang.io_fsync(fd: Int32) -> Int32
//   lang.io_ftruncate(fd: Int32, length: Int64) -> Int32
//
// File metadata:
//   lang.io_stat(path: lang.ptr[lang.u8]) -> lang.stat_result
//   lang.io_fstat(fd: Int32) -> lang.stat_result
//   lang.stat_size(stat: lang.stat_result) -> Int64
//   lang.stat_mode(stat: lang.stat_result) -> Int32
//   lang.stat_is_file(stat: lang.stat_result) -> Bool
//   lang.stat_is_dir(stat: lang.stat_result) -> Bool
//   lang.stat_is_symlink(stat: lang.stat_result) -> Bool
//   lang.stat_mtime(stat: lang.stat_result) -> Int64
//   lang.stat_atime(stat: lang.stat_result) -> Int64
//   lang.stat_ctime(stat: lang.stat_result) -> Int64
//
// File system operations:
//   lang.io_unlink(path: lang.ptr[lang.u8]) -> Int32
//   lang.io_rename(old: lang.ptr[lang.u8], new: lang.ptr[lang.u8]) -> Int32
//
// Error handling:
//   lang.io_errno() -> Int32
//
// Usage:
//   import std.io
//
//   // Read a file
//   let contents = try io.readToString(path: "hello.txt")
//
//   // Write a file
//   try io.writeString(path: "output.txt", contents: "Hello, World!")
//
//   // Use File directly
//   let file = try File.open(path: "data.txt")
//   var buffer: [UInt8; 1024] = [0; 1024]
//   let n = try file.read(buffer: buffer.asSlice())
//
//   // Buffered I/O
//   let reader = BufReader(reader: file)
//   for line in reader.lines() {
//       println(message: try line)
//   }
//
//   // Standard I/O
//   let name = try prompt(message: "Enter your name: ")
//   try println(message: "Hello, " + name + "!")

module std.io

// Error types
public import std.io.error.(
    IoError,
    IoResult,
    ErrorKind
)

// File descriptor types
public import std.io.fd.(
    OpenFlags,
    FileMode,
    SeekFrom,
    RawFd,
    OwnedFd,
    BorrowedFd
)

// Stream protocols
public import std.io.stream.(
    Reader,
    Writer,
    Seek,
    ReadSeek,
    WriteSeek,
    ReadWrite,
    BytesReader,
    ChainReader,
    TakeReader,
    EmptyReader,
    RepeatReader,
    SinkWriter,
    Cursor,
    copy
)

// File I/O
public import std.io.file.(
    OpenOptions,
    File,
    Metadata,
    readToString,
    readBytes,
    writeString,
    writeBytes,
    appendString,
    metadata,
    exists,
    isFile,
    isDir,
    remove,
    rename,
    copyFile
)

// Buffered I/O
public import std.io.buffered.(
    BufferedReader,
    BufferedWriter,
    LineWriter,
    LinesReader,
    BufReader,
    BufWriter
)

// Standard I/O
public import std.io.stdio.(
    Stdin,
    StdinLock,
    Stdout,
    StdoutLock,
    Stderr,
    StderrLock,
    stdin,
    stdout,
    stderr,
    print,
    println,
    eprint,
    eprintln,
    readLine,
    readInt,
    readFloat,
    prompt
)
