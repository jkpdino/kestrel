// ===== Fileprivate Visibility Tests =====

// Library with fileprivate visibility
module FileprivateLib

fileprivate class FileOnlyClass {}
public class PublicClass {}

// ===== Different file attempting fileprivate import (should fail) =====

module OtherFileprivateModule

// Try to import fileprivate - should fail
import FileprivateLib.(FileOnlyClass)

class Consumer {}

// ===== Internal Visibility Tests =====

// Library with internal visibility
module InternalLib

internal class InternalClass {}
public class PublicClass {}

// Same module - should see internal
class UsesInternal {}

// ===== Different module attempting internal import (should fail) =====

module OtherInternalModule

// Try to import internal - should fail
import InternalLib.(InternalClass)

class Consumer {}
