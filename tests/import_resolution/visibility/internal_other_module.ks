// Different module - should NOT see internal
module OtherModule

// Try to import internal - this should fail (but we treat internal as visible for now)
import InternalLib.(InternalClass)

class Consumer {}
