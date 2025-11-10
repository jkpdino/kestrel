// Unicode identifiers
module TypeAliasEdgeCases

type café = Coffee;
type 世界 = World;
type αβγ = Greek;
type Привет = Hello;
type 你好 = ChineseHello;
type مرحبا = ArabicHello;

// Very long identifiers
type VeryLongTypeAliasNameThatGoesOnAndOnAndOnAndOnAndOn = SomeType;
type AnotherExtremelyLongIdentifierJustToTestTheLimits = AnotherType;

// Single character identifiers
type A = Foo;
type B = Bar;
type C = Baz;
type X = Unknown;
type Y = YType;
type Z = ZType;

// Identifiers with underscores
type _leading = LeadingUnderscore;
type trailing_ = TrailingUnderscore;
type _both_ = BothUnderscore;
type __double = DoubleUnderscore;
type mid_dle = MiddleUnderscore;

// Identifiers with numbers
type Type1 = First;
type Type2 = Second;
type Type123 = OneTwoThree;
type Alias0 = Zero;
type Map2D = TwoDimensionalMap;
type Vector3D = ThreeDimensionalVector;

// Mixed scripts
type hello世界 = MixedScript;
type café123 = MixedWithNumbers;
type αβγ_delta = GreekWithUnderscore;
