module Token (
  Token(..)
) where

data Token =
    UNIT
  | ID String
  | NUM Int
  | FN
  | DOT
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | PLUS
  | MINUS
  deriving (Show, Eq)
