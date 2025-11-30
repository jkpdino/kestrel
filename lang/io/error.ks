// I/O Error types

module io.error

import std.(Error, Result, String)
import io.libc

// I/O Error
public struct Error: std.Error {
    var code: Int32

    public init(code: Int32) {
        self.code = code
    }

    // Create from current errno
    public static func last() -> Error {
        Error(code: libc.errno())
    }

    public var description: String {
        match self.code {
            1 => "operation not permitted",
            2 => "no such file or directory",
            4 => "interrupted",
            5 => "i/o error",
            9 => "bad file descriptor",
            11 => "would block",
            12 => "out of memory",
            13 => "permission denied",
            17 => "file exists",
            20 => "not a directory",
            21 => "is a directory",
            22 => "invalid argument",
            28 => "no space left",
            32 => "broken pipe",
            _ => "unknown error"
        }
    }

    public var errno: Int32 {
        self.code
    }
}

// Common error constructors
extension Error {
    public static let notFound = Error(code: 2)
    public static let permissionDenied = Error(code: 13)
    public static let alreadyExists = Error(code: 17)
    public static let invalidInput = Error(code: 22)
    public static let wouldBlock = Error(code: 11)
    public static let interrupted = Error(code: 4)
    public static let brokenPipe = Error(code: 32)

    public static func custom(message: String) -> Error {
        // Use a high errno for custom errors
        Error(code: 1000)
    }
}

// Result type alias
public type Result[T] = std.Result[T, Error]
