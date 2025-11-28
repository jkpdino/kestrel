// Consumer module that imports from Library
module Consumer

// Import all visible symbols from Library
import Library

// This should work - PublicClass is public
class UsesPublic {}

// This should fail - PrivateClass is not visible
// class UsesPrivate {}
