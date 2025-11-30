// File descriptor - low-level wrapper around OS file descriptors
//
// This module provides direct access to libc file operations via lang intrinsics.
// The lang.io_* intrinsics map to the following libc functions:
//   lang.io_open(path, flags, mode) -> Int32      // open()
//   lang.io_close(fd) -> Int32                     // close()
//   lang.io_read(fd, buf, count) -> Int           // read()
//   lang.io_write(fd, buf, count) -> Int          // write()
//   lang.io_seek(fd, offset, whence) -> Int64     // lseek()
//   lang.io_fsync(fd) -> Int32                    // fsync()
//   lang.io_ftruncate(fd, length) -> Int32        // ftruncate()
//   lang.io_errno() -> Int32                      // errno

// Open flags (matching POSIX)
public struct OpenFlags {
    private var bits: Int32

    public init(bits: Int32) {
        self.bits = bits
    }

    public var rawValue: Int32 {
        self.bits
    }

    // Basic flags
    public static let readOnly = OpenFlags(bits: 0)           // O_RDONLY
    public static let writeOnly = OpenFlags(bits: 1)          // O_WRONLY
    public static let readWrite = OpenFlags(bits: 2)          // O_RDWR

    // Additional flags (these vary by platform, using common Linux/macOS values)
    public static let create = OpenFlags(bits: 0x200)         // O_CREAT
    public static let truncate = OpenFlags(bits: 0x400)       // O_TRUNC
    public static let append = OpenFlags(bits: 0x8)           // O_APPEND
    public static let exclusive = OpenFlags(bits: 0x800)      // O_EXCL
    public static let nonBlock = OpenFlags(bits: 0x4)         // O_NONBLOCK

    // Combine flags
    public func or(other: OpenFlags) -> OpenFlags {
        OpenFlags(bits: self.bits | other.bits)
    }
}

// File mode (permissions)
public struct FileMode {
    private var bits: Int32

    public init(bits: Int32) {
        self.bits = bits
    }

    public var rawValue: Int32 {
        self.bits
    }

    // Common permission modes
    public static let ownerRead = FileMode(bits: 0o400)
    public static let ownerWrite = FileMode(bits: 0o200)
    public static let ownerExecute = FileMode(bits: 0o100)
    public static let ownerAll = FileMode(bits: 0o700)

    public static let groupRead = FileMode(bits: 0o040)
    public static let groupWrite = FileMode(bits: 0o020)
    public static let groupExecute = FileMode(bits: 0o010)
    public static let groupAll = FileMode(bits: 0o070)

    public static let otherRead = FileMode(bits: 0o004)
    public static let otherWrite = FileMode(bits: 0o002)
    public static let otherExecute = FileMode(bits: 0o001)
    public static let otherAll = FileMode(bits: 0o007)

    // Common combinations
    public static let defaultFile = FileMode(bits: 0o644)     // rw-r--r--
    public static let defaultDir = FileMode(bits: 0o755)      // rwxr-xr-x
    public static let private_ = FileMode(bits: 0o600)        // rw-------

    public func or(other: FileMode) -> FileMode {
        FileMode(bits: self.bits | other.bits)
    }
}

// Seek position
public enum SeekFrom {
    case Start(Int64)      // SEEK_SET - offset from start
    case Current(Int64)    // SEEK_CUR - offset from current position
    case End(Int64)        // SEEK_END - offset from end

    public var whence: Int32 {
        match self {
            .Start(_) => 0,
            .Current(_) => 1,
            .End(_) => 2
        }
    }

    public var offset: Int64 {
        match self {
            .Start(let o) => o,
            .Current(let o) => o,
            .End(let o) => o
        }
    }
}

// RawFd - raw file descriptor
public struct RawFd: Equatable {
    private var fd: Int32

    public init(fd: Int32) {
        self.fd = fd
    }

    public var rawValue: Int32 {
        self.fd
    }

    public var isValid: Bool {
        self.fd >= 0
    }

    // Standard file descriptors
    public static let stdin = RawFd(fd: 0)
    public static let stdout = RawFd(fd: 1)
    public static let stderr = RawFd(fd: 2)

    public static let invalid = RawFd(fd: -1)

    public func equals(other: RawFd) -> Bool {
        self.fd == other.fd
    }
}

// OwnedFd - owned file descriptor that closes on drop
public struct OwnedFd: NonCopyable {
    private var fd: RawFd

    public init(fd: RawFd) {
        self.fd = fd
    }

    public init(raw: Int32) {
        self.fd = RawFd(fd: raw)
    }

    public var raw: RawFd {
        self.fd
    }

    public var rawValue: Int32 {
        self.fd.rawValue
    }

    public var isValid: Bool {
        self.fd.isValid
    }

    // Open a file descriptor
    public static func open(path: String, flags: OpenFlags, mode: FileMode) -> IoResult[OwnedFd] {
        let pathPtr = path.bytes.pointer.asRaw().raw
        let fd = lang.io_open(pathPtr, flags.rawValue, mode.rawValue)
        if fd < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        .Ok(OwnedFd(raw: fd))
    }

    // Read bytes into buffer
    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        let count = lang.io_read(self.fd.rawValue, buffer.pointer.asRaw().raw, buffer.count)
        if count < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        .Ok(count)
    }

    // Write bytes from buffer
    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        let count = lang.io_write(self.fd.rawValue, buffer.pointer.asRaw().raw, buffer.count)
        if count < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        .Ok(count)
    }

    // Seek to position
    public func seek(pos: SeekFrom) -> IoResult[Int64] {
        let result = lang.io_seek(self.fd.rawValue, pos.offset, pos.whence)
        if result < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        .Ok(result)
    }

    // Sync to disk
    public func sync() -> IoResult[Unit] {
        let result = lang.io_fsync(self.fd.rawValue)
        if result < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        .Ok(())
    }

    // Truncate file
    public func truncate(length: Int64) -> IoResult[Unit] {
        let result = lang.io_ftruncate(self.fd.rawValue, length)
        if result < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        .Ok(())
    }

    // Close the file descriptor
    public func close() -> IoResult[Unit] {
        if self.fd.isValid {
            let result = lang.io_close(self.fd.rawValue)
            self.fd = RawFd.invalid
            if result < 0 {
                let errno = lang.io_errno()
                return .Err(IoError.fromErrno(errno: errno))
            }
        }
        .Ok(())
    }

    // Take ownership of the raw fd (prevents close on drop)
    public func take() -> RawFd {
        let result = self.fd
        self.fd = RawFd.invalid
        result
    }

    // Destructor - close the fd
    deinit {
        if self.fd.isValid {
            _ = lang.io_close(self.fd.rawValue)
        }
    }
}

// BorrowedFd - borrowed reference to a file descriptor
public struct BorrowedFd {
    private var fd: RawFd

    public init(fd: RawFd) {
        self.fd = fd
    }

    public init(raw: Int32) {
        self.fd = RawFd(fd: raw)
    }

    public var raw: RawFd {
        self.fd
    }

    public var rawValue: Int32 {
        self.fd.rawValue
    }

    // Read bytes into buffer
    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        let count = lang.io_read(self.fd.rawValue, buffer.pointer.asRaw().raw, buffer.count)
        if count < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        .Ok(count)
    }

    // Write bytes from buffer
    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        let count = lang.io_write(self.fd.rawValue, buffer.pointer.asRaw().raw, buffer.count)
        if count < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        .Ok(count)
    }
}

// Extension to get BorrowedFd from OwnedFd
extension OwnedFd {
    public func borrow() -> BorrowedFd {
        BorrowedFd(fd: self.fd)
    }
}
