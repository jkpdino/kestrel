// Library module with various visibility levels
module Library

// Public - visible everywhere
public class PublicClass {}

// Internal - visible within the module
internal class InternalClass {}

// Private - not visible outside this file
private class PrivateClass {}

// Default visibility (internal)
class DefaultClass {}

// Public type alias
public type PublicAlias = PublicClass;

// Private type alias
private type PrivateAlias = PrivateClass;
