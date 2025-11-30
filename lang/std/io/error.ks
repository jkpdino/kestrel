// IO Error types

// IoError - represents I/O operation failures
public struct IoError: Error {
    private var code: Int32
    private var message: String

    public init(code: Int32, message: String) {
        self.code = code
        self.message = message
    }

    public var description: String {
        self.message
    }

    public var errorCode: Int32 {
        self.code
    }

    // Common error constructors
    public static func notFound(path: String) -> IoError {
        IoError(code: 2, message: "No such file or directory: " + path)
    }

    public static func permissionDenied(path: String) -> IoError {
        IoError(code: 13, message: "Permission denied: " + path)
    }

    public static func alreadyExists(path: String) -> IoError {
        IoError(code: 17, message: "File exists: " + path)
    }

    public static func invalidInput(reason: String) -> IoError {
        IoError(code: 22, message: "Invalid argument: " + reason)
    }

    public static func wouldBlock() -> IoError {
        IoError(code: 11, message: "Resource temporarily unavailable")
    }

    public static func interrupted() -> IoError {
        IoError(code: 4, message: "Interrupted system call")
    }

    public static func unexpectedEof() -> IoError {
        IoError(code: 0, message: "Unexpected end of file")
    }

    public static func brokenPipe() -> IoError {
        IoError(code: 32, message: "Broken pipe")
    }

    public static func fromErrno(errno: Int32) -> IoError {
        let message = match errno {
            1 => "Operation not permitted",
            2 => "No such file or directory",
            4 => "Interrupted system call",
            5 => "I/O error",
            9 => "Bad file descriptor",
            11 => "Resource temporarily unavailable",
            12 => "Cannot allocate memory",
            13 => "Permission denied",
            17 => "File exists",
            20 => "Not a directory",
            21 => "Is a directory",
            22 => "Invalid argument",
            23 => "Too many open files in system",
            24 => "Too many open files",
            27 => "File too large",
            28 => "No space left on device",
            30 => "Read-only file system",
            32 => "Broken pipe",
            _ => "Unknown error"
        }
        IoError(code: errno, message: message)
    }
}

// IoResult - convenience alias for Result with IoError
public type IoResult[T] = Result[T, IoError]

// ErrorKind - categorizes I/O errors
public enum ErrorKind {
    case NotFound
    case PermissionDenied
    case AlreadyExists
    case InvalidInput
    case InvalidData
    case WouldBlock
    case Interrupted
    case UnexpectedEof
    case BrokenPipe
    case Other

    public static func from(errno: Int32) -> ErrorKind {
        match errno {
            2 => .NotFound,
            13 => .PermissionDenied,
            17 => .AlreadyExists,
            22 => .InvalidInput,
            11 => .WouldBlock,
            4 => .Interrupted,
            32 => .BrokenPipe,
            _ => .Other
        }
    }
}

extension IoError {
    public var kind: ErrorKind {
        ErrorKind.from(errno: self.code)
    }
}

extension IoError: Equatable {
    public func equals(other: IoError) -> Bool {
        self.code == other.code
    }
}
