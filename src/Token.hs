module Token (
  Token(..)
) where


-- |A token
data Token =
    UNIT
  | ID String
  | NUM Int
  | BOOL Bool
  | FN
  | DOT
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | PLUS
  | MINUS
  | LT
  | EQ
  deriving (Show, Eq)
