// Basic type alias usage
module TypeAliasBasic

type Simple = Int;

// Type alias with visibility modifiers
public type PublicAlias = String;
private type PrivateAlias = Bool;
internal type InternalAlias = Float;
fileprivate type FilePrivateAlias = Char;

// Type aliases with different naming patterns
type CamelCaseAlias = SomeType;
type snake_case_alias = AnotherType;
type SCREAMING_SNAKE = YetAnotherType;
type MixedCase123 = TypeWithNumbers;

// Simple aliases for common types
type Result = Either;
type Maybe = Optional;
type List = Array;
