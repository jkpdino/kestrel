// Edge cases for class declarations

// Single character class name
class A { }

// Mixed visibility patterns
public class PublicOne { }
private class PrivateOne { }
internal class InternalOne { }
fileprivate class FileprivateOne { }
class DefaultOne { }

// Class names with underscores
class _LeadingUnderscore { }
class TrailingUnderscore_ { }
class _BothUnderscores_ { }

// Class names with numbers (not at start, as that would be invalid)
class Class1 { }
class MyClass2000 { }
