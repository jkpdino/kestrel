// Test that importing private symbols fails
module ImportPrivate

// This should fail - PrivateClass is private
import Library.(PrivateClass)

// This should also fail - PrivateAlias is private
import Library.(PrivateAlias)

class MyClass {}
