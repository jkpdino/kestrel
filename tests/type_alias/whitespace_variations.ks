// Testing various whitespace patterns
module WhitespaceVariations

// Standard formatting
type Standard = Normal;

// Extra spaces
type   ExtraSpaces   =   Spaced   ;

// Minimal spacing (but still valid)
type Minimal=Compact;

// Multiple blank lines between declarations


type WithBlankLines = Type1;


type MoreBlankLines = Type2;


// Mixed indentation (though type aliases are top-level)
type FirstLine = A;
    type Indented = B;
        type MoreIndented = C;
type BackToNormal = D;

// Trailing whitespace (invisible but tests robustness)
type Trailing = Type;
type Another = Type;

// Comments and whitespace
type Before = Type; // Comment after
// Comment before
type After = Type;

/* Block comment before */
type BlockComment = Type;
type AnotherBlock = Type; /* Comment after */

// All visibility modifiers with various spacing
public  type  Public1  =  Type  ;
private type Private1 = Type;
internal   type   Internal1   =   Type;
fileprivate type FilePrivate1 = Type;
