// Edge cases for type aliases
module TypeAliasEdgeCases

// Single character identifiers
type A = Foo
type B = Bar

// Identifiers with underscores
type _leading = LeadingUnderscore
type trailing_ = TrailingUnderscore
type _both_ = BothUnderscore
type __double = DoubleUnderscore
type mid_dle = MiddleUnderscore

// Identifiers with numbers
type Type1 = First
type Type2 = Second
type Type123 = OneTwoThree
type Alias0 = Zero
type Map2D = TwoDimensionalMap
type Vector3D = ThreeDimensionalVector

// Note: Unicode identifiers and very long names are tested in edge_cases.ks
