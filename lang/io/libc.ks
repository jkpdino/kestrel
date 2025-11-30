// libc bindings for I/O
//
// This module provides raw bindings to libc I/O functions.
// These are thin wrappers around lang.libc_* intrinsics.
//
// The compiler must provide these intrinsics that map directly to libc:
//
//   lang.libc_open(path: Pointer[UInt8], flags: Int32, mode: Int32) -> Int32
//   lang.libc_close(fd: Int32) -> Int32
//   lang.libc_read(fd: Int32, buf: Pointer[UInt8], count: UInt) -> Int
//   lang.libc_write(fd: Int32, buf: Pointer[UInt8], count: UInt) -> Int
//   lang.libc_lseek(fd: Int32, offset: Int64, whence: Int32) -> Int64
//   lang.libc_errno() -> Int32
//
// All functions return -1 on error and set errno.

module io.libc

// File descriptor type (just an int)
public type Fd = Int32

// Standard file descriptors
public let STDIN: Fd = 0
public let STDOUT: Fd = 1
public let STDERR: Fd = 2

// Open flags (POSIX)
public let O_RDONLY: Int32 = 0x0000
public let O_WRONLY: Int32 = 0x0001
public let O_RDWR: Int32 = 0x0002
public let O_CREAT: Int32 = 0x0200
public let O_TRUNC: Int32 = 0x0400
public let O_APPEND: Int32 = 0x0008
public let O_EXCL: Int32 = 0x0800

// Seek whence
public let SEEK_SET: Int32 = 0
public let SEEK_CUR: Int32 = 1
public let SEEK_END: Int32 = 2

// Default file mode (rw-r--r--)
public let MODE_DEFAULT: Int32 = 0o644

// Open a file
// Returns file descriptor on success, -1 on error
public func open(path: Pointer[UInt8], flags: Int32, mode: Int32) -> Fd {
    lang.libc_open(path, flags, mode)
}

// Close a file descriptor
// Returns 0 on success, -1 on error
public func close(fd: Fd) -> Int32 {
    lang.libc_close(fd)
}

// Read from file descriptor
// Returns bytes read on success, -1 on error, 0 on EOF
public func read(fd: Fd, buf: Pointer[UInt8], count: UInt) -> Int {
    lang.libc_read(fd, buf, count)
}

// Write to file descriptor
// Returns bytes written on success, -1 on error
public func write(fd: Fd, buf: Pointer[UInt8], count: UInt) -> Int {
    lang.libc_write(fd, buf, count)
}

// Seek in file
// Returns new offset on success, -1 on error
public func lseek(fd: Fd, offset: Int64, whence: Int32) -> Int64 {
    lang.libc_lseek(fd, offset, whence)
}

// Get last error number
public func errno() -> Int32 {
    lang.libc_errno()
}
