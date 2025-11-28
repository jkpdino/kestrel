// Different file - should NOT see fileprivate
module OtherFileprivateModule

// Try to import fileprivate - this should fail
import FileprivateLib.(FileOnlyClass)

class Consumer {}
