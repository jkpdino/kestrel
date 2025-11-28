// ===== Unicode Identifiers =====
// Latin with accents
module Café
module Résumé.Naïve

// Greek letters
module αβγ.δεζ.ηθι

// Chinese characters
module 世界
module 你好

// Arabic script
module مرحبا

// Mixed scripts
module Hello.世界.مرحبا

// Unicode in class names
class Café { }
class αβγ { }
class 世界 { }

// Unicode in function names
fn café() {}
fn 世界() {}
fn привет() {}
fn αβγ() {}

// Unicode in type aliases
type café = Coffee
type 世界 = World
type αβγ = Greek
type Привет = Hello
type 你好 = ChineseHello
type مرحبا = ArabicHello

// ===== Very Long Identifiers =====

// Long class name
class VeryLongClassNameThatIsStillValid { }

// Long function name
fn thisIsAVeryLongFunctionNameThatIsStillValidButUnusual() {}

// Long parameter name
fn withParams(veryLongParameterName: String) {}
fn anotherFunction(anotherLongParameterName: Int) {}

// Long function with return type
fn veryLongFunctionNameWithParametersAndReturnType() -> String {}

// Long type alias
type VeryLongTypeAliasNameThatGoesOnAndOnAndOnAndOnAndOn = SomeType
type AnotherExtremelyLongIdentifierJustToTestTheLimits = AnotherType

// ===== Underscore Patterns =====

// In class names
class _LeadingUnderscore { }
class TrailingUnderscore_ { }
class _BothUnderscores_ { }

// In module paths
module My_Module.Sub_Module

// In type aliases
type _leading = LeadingUnderscore
type trailing_ = TrailingUnderscore
type _both_ = BothUnderscore
type __double = DoubleUnderscore
type mid_dle = MiddleUnderscore

// ===== Numbers in Identifiers =====

// Class names with numbers
class Class1 { }
class MyClass2000 { }

// Type aliases with numbers
type Type1 = First
type Type2 = Second
type Type123 = OneTwoThree
type Alias0 = Zero
type Map2D = TwoDimensionalMap
type Vector3D = ThreeDimensionalVector

// ===== Single Character Identifiers =====

class A { }

module A

fn x() {}

type A = Foo
type B = Bar
type C = Baz
type X = Unknown
type Y = YType
type Z = ZType

// ===== Mixed Features =====

// Unicode with underscores
type αβγ_delta = GreekWithUnderscore

// Unicode with numbers
type café123 = MixedWithNumbers
type hello世界 = MixedScript

// Long identifiers with mixed case
module MyApp.UserModule.AuthController
