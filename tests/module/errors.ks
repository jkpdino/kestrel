// Expected error cases for module declarations
module Test.Module.Errors

// ERROR: Should fail - no module path provided
// module

// ERROR: Should fail - trailing dot
// module A.B.

// ERROR: Should fail - leading dot
// module .A.B

// ERROR: Should fail - double dot
// module A..B

// ERROR: Should fail - module keyword used twice
// module module A.B

// ERROR: Should fail - numbers cannot start identifiers
// module 123.ABC

// ERROR: Should fail - special characters in module name
// module A.B$.C

// ERROR: Should fail - spaces in module path
// module A. B.C

// ERROR: Should fail - empty segment
// module A..C
