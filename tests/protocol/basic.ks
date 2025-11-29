module Protocol.Basic

// Basic protocol declarations

// Simple protocol without visibility modifier
protocol Drawable { }

// Protocol with public visibility
public protocol Serializable { }

// Protocol with private visibility
private protocol InternalHelper { }

// Protocol with internal visibility
internal protocol PackageVisible { }

// Protocol with fileprivate visibility
fileprivate protocol FileLocal { }

// Multiple protocols in one file
protocol First { }
protocol Second { }
protocol Third { }
