// File - high-level file I/O
//
// Provides a safe, high-level interface for file operations built on OwnedFd.

// OpenOptions - builder for configuring file open options
public struct OpenOptions {
    private var read_: Bool
    private var write_: Bool
    private var append_: Bool
    private var truncate_: Bool
    private var create_: Bool
    private var createNew_: Bool
    private var mode_: FileMode

    public init() {
        self.read_ = false
        self.write_ = false
        self.append_ = false
        self.truncate_ = false
        self.create_ = false
        self.createNew_ = false
        self.mode_ = FileMode.defaultFile
    }

    // Builder methods
    public func read(enabled: Bool) -> OpenOptions {
        var opts = self
        opts.read_ = enabled
        opts
    }

    public func write(enabled: Bool) -> OpenOptions {
        var opts = self
        opts.write_ = enabled
        opts
    }

    public func append(enabled: Bool) -> OpenOptions {
        var opts = self
        opts.append_ = enabled
        opts
    }

    public func truncate(enabled: Bool) -> OpenOptions {
        var opts = self
        opts.truncate_ = enabled
        opts
    }

    public func create(enabled: Bool) -> OpenOptions {
        var opts = self
        opts.create_ = enabled
        opts
    }

    public func createNew(enabled: Bool) -> OpenOptions {
        var opts = self
        opts.createNew_ = enabled
        opts
    }

    public func mode(mode: FileMode) -> OpenOptions {
        var opts = self
        opts.mode_ = mode
        opts
    }

    // Compute open flags
    private func flags() -> OpenFlags {
        var flags = if self.read_ and self.write_ {
            OpenFlags.readWrite
        } else if self.write_ {
            OpenFlags.writeOnly
        } else {
            OpenFlags.readOnly
        }

        if self.append_ {
            flags = flags.or(other: OpenFlags.append)
        }
        if self.truncate_ {
            flags = flags.or(other: OpenFlags.truncate)
        }
        if self.create_ {
            flags = flags.or(other: OpenFlags.create)
        }
        if self.createNew_ {
            flags = flags.or(other: OpenFlags.create).or(other: OpenFlags.exclusive)
        }

        flags
    }

    // Open a file with these options
    public func open(path: String) -> IoResult[File] {
        let fd = try OwnedFd.open(path: path, flags: self.flags(), mode: self.mode_)
        .Ok(File(fd: fd))
    }
}

// File - represents an open file
public struct File: Reader, Writer, Seek, ReadSeek, WriteSeek, NonCopyable {
    private var fd: OwnedFd

    // Private constructor - use File.open or OpenOptions
    private init(fd: OwnedFd) {
        self.fd = fd
    }

    // Open a file for reading
    public static func open(path: String) -> IoResult[File] {
        OpenOptions()
            .read(enabled: true)
            .open(path: path)
    }

    // Create a new file for writing (truncates if exists)
    public static func create(path: String) -> IoResult[File] {
        OpenOptions()
            .write(enabled: true)
            .create(enabled: true)
            .truncate(enabled: true)
            .open(path: path)
    }

    // Create a new file, failing if it exists
    public static func createNew(path: String) -> IoResult[File] {
        OpenOptions()
            .write(enabled: true)
            .createNew(enabled: true)
            .open(path: path)
    }

    // Open file for both reading and writing
    public static func openReadWrite(path: String) -> IoResult[File] {
        OpenOptions()
            .read(enabled: true)
            .write(enabled: true)
            .open(path: path)
    }

    // Open file for appending
    public static func openAppend(path: String) -> IoResult[File] {
        OpenOptions()
            .write(enabled: true)
            .append(enabled: true)
            .create(enabled: true)
            .open(path: path)
    }

    // Get custom open options builder
    public static func options() -> OpenOptions {
        OpenOptions()
    }

    // Reader implementation
    public func read(buffer: Slice[UInt8]) -> IoResult[Int] {
        self.fd.read(buffer: buffer)
    }

    // Writer implementation
    public func write(buffer: Slice[UInt8]) -> IoResult[Int] {
        self.fd.write(buffer: buffer)
    }

    public func flush() -> IoResult[Unit] {
        self.fd.sync()
    }

    // Seek implementation
    public func seek(pos: SeekFrom) -> IoResult[Int64] {
        self.fd.seek(pos: pos)
    }

    // File-specific operations

    // Sync all data and metadata to disk
    public func syncAll() -> IoResult[Unit] {
        self.fd.sync()
    }

    // Sync data (not necessarily metadata) to disk
    public func syncData() -> IoResult[Unit] {
        // Note: could use fdatasync on Linux, but fsync is more portable
        self.fd.sync()
    }

    // Truncate or extend the file
    public func setLen(size: Int64) -> IoResult[Unit] {
        self.fd.truncate(length: size)
    }

    // Get file metadata
    public func metadata() -> IoResult[Metadata] {
        let result = lang.io_fstat(self.fd.rawValue)
        if result < 0 {
            let errno = lang.io_errno()
            return .Err(IoError.fromErrno(errno: errno))
        }
        // Parse stat result into Metadata
        .Ok(Metadata.fromStat(result))
    }

    // Read the entire file contents into a string
    public func readToString() -> IoResult[String] {
        var bytes: Array[UInt8] = []
        _ = try self.readToEnd(buffer: bytes)
        // Note: This assumes valid UTF-8. A production implementation should validate.
        .Ok(String(utf8: bytes.asSlice()))
    }

    // Borrow the underlying file descriptor
    public func asFd() -> BorrowedFd {
        self.fd.borrow()
    }

    // Take ownership of the underlying fd
    public func intoFd() -> OwnedFd {
        self.fd
    }
}

// Metadata - file metadata from stat
public struct Metadata {
    private var size_: Int64
    private var isFile_: Bool
    private var isDir_: Bool
    private var isSymlink_: Bool
    private var mode_: FileMode
    private var modified_: Int64   // Unix timestamp
    private var accessed_: Int64   // Unix timestamp
    private var created_: Int64    // Unix timestamp (may be 0 if unavailable)

    public init(
        size: Int64,
        isFile: Bool,
        isDir: Bool,
        isSymlink: Bool,
        mode: FileMode,
        modified: Int64,
        accessed: Int64,
        created: Int64
    ) {
        self.size_ = size
        self.isFile_ = isFile
        self.isDir_ = isDir
        self.isSymlink_ = isSymlink
        self.mode_ = mode
        self.modified_ = modified
        self.accessed_ = accessed
        self.created_ = created
    }

    public static func fromStat(statResult: lang.stat_result) -> Metadata {
        Metadata(
            size: lang.stat_size(statResult),
            isFile: lang.stat_is_file(statResult),
            isDir: lang.stat_is_dir(statResult),
            isSymlink: lang.stat_is_symlink(statResult),
            mode: FileMode(bits: lang.stat_mode(statResult)),
            modified: lang.stat_mtime(statResult),
            accessed: lang.stat_atime(statResult),
            created: lang.stat_ctime(statResult)
        )
    }

    public var size: Int64 { self.size_ }
    public var isFile: Bool { self.isFile_ }
    public var isDir: Bool { self.isDir_ }
    public var isSymlink: Bool { self.isSymlink_ }
    public var permissions: FileMode { self.mode_ }
    public var modified: Int64 { self.modified_ }
    public var accessed: Int64 { self.accessed_ }
    public var created: Int64 { self.created_ }
}

// Convenience functions for common file operations

// Read entire file to string
public func readToString(path: String) -> IoResult[String] {
    let file = try File.open(path: path)
    file.readToString()
}

// Read entire file to bytes
public func readBytes(path: String) -> IoResult[Array[UInt8]] {
    let file = try File.open(path: path)
    var bytes: Array[UInt8] = []
    _ = try file.readToEnd(buffer: bytes)
    .Ok(bytes)
}

// Write string to file (creates or truncates)
public func writeString(path: String, contents: String) -> IoResult[Unit] {
    let file = try File.create(path: path)
    file.writeString(string: contents)
}

// Write bytes to file (creates or truncates)
public func writeBytes(path: String, contents: Slice[UInt8]) -> IoResult[Unit] {
    let file = try File.create(path: path)
    file.writeAll(buffer: contents)
}

// Append string to file
public func appendString(path: String, contents: String) -> IoResult[Unit] {
    let file = try File.openAppend(path: path)
    file.writeString(string: contents)
}

// Get file metadata
public func metadata(path: String) -> IoResult[Metadata] {
    let pathPtr = path.bytes.pointer.asRaw().raw
    let result = lang.io_stat(pathPtr)
    if result < 0 {
        let errno = lang.io_errno()
        return .Err(IoError.fromErrno(errno: errno))
    }
    .Ok(Metadata.fromStat(result))
}

// Check if path exists
public func exists(path: String) -> Bool {
    match metadata(path: path) {
        .Ok(_) => true,
        .Err(_) => false
    }
}

// Check if path is a file
public func isFile(path: String) -> Bool {
    match metadata(path: path) {
        .Ok(let meta) => meta.isFile,
        .Err(_) => false
    }
}

// Check if path is a directory
public func isDir(path: String) -> Bool {
    match metadata(path: path) {
        .Ok(let meta) => meta.isDir,
        .Err(_) => false
    }
}

// Remove a file
public func remove(path: String) -> IoResult[Unit] {
    let pathPtr = path.bytes.pointer.asRaw().raw
    let result = lang.io_unlink(pathPtr)
    if result < 0 {
        let errno = lang.io_errno()
        return .Err(IoError.fromErrno(errno: errno))
    }
    .Ok(())
}

// Rename/move a file
public func rename(from oldPath: String, to newPath: String) -> IoResult[Unit] {
    let oldPtr = oldPath.bytes.pointer.asRaw().raw
    let newPtr = newPath.bytes.pointer.asRaw().raw
    let result = lang.io_rename(oldPtr, newPtr)
    if result < 0 {
        let errno = lang.io_errno()
        return .Err(IoError.fromErrno(errno: errno))
    }
    .Ok(())
}

// Copy a file
public func copyFile(from src: String, to dst: String) -> IoResult[Int64] {
    var srcFile = try File.open(path: src)
    var dstFile = try File.create(path: dst)
    copy(from: srcFile, to: dstFile)
}
