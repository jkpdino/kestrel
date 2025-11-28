// Test importing non-existent symbols
module ImportNonexistent

// This should fail - NonExistent doesn't exist in Library
import Library.(NonExistent)

class MyClass {}
