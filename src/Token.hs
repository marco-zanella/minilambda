module Token (
  Token(..)
) where


-- |A token
data Token =
    UNIT       -- ^ Unit value
  | ID String  -- ^ Identifier
  | NUM Int    -- ^ Numeric value
  | BOOL Bool  -- ^ Boolean value
  | FN         -- ^ Function symbol
  | DOT        -- ^ Dot symbol
  | IF         -- ^ If keyword
  | THEN       -- ^ Then keyword
  | ELSE       -- ^ Else keyword
  | LPAR       -- ^ Left parenthesis '('
  | RPAR       -- ^ Right parenthesis ')'
  | PLUS       -- ^ Plus symbol '+'
  | MINUS      -- ^ Minus symbol '-'
  | LT         -- ^ Less than symbol '<'
  | EQ         -- ^ Equality symbol '='
  deriving (Show, Eq)
