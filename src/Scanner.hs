module Scanner (
  scan
) where

import Token
import Data.Char


-- |Converts a digit into an interger
digit :: Char -> Int
digit '1' = 1
digit '2' = 2
digit '3' = 3
digit '4' = 4
digit '5' = 5
digit '6' = 6
digit '7' = 7
digit '8' = 8
digit '9' = 9
digit _ = 0


-- |Parses a string converting it into a number
num :: Int -> String -> (Int, String)
num n (c : s) =
  if isDigit c then
    num (n * 10 + digitToInt c) s
  else
    (n, c : s)
num n [] = (n, [])


-- |Recognizes an identifier
id :: String -> String -> (String, String)
id i (c : s) =
  if isAlpha c || isDigit c then
    Scanner.id (i ++ [c]) s
  else
    (i, c : s)
id i [] = (i, [])


-- |Scans a string converting it to a list of tokens
scan :: String -> [Token]

scan [] = []

scan ('u':'n':'i':'t' : string) = UNIT : scan string

scan ('t':'r':'u':'e' : string) = BOOL True : scan string
scan ('f':'a':'l':'s':'e' : string) = BOOL False : scan string

scan ('f':'n' : string) = FN : scan string

scan ('.' : string) = DOT : scan string

scan ('i':'f' : string) = IF : scan string
scan ('t':'h':'e':'n' : string) = THEN : scan string
scan ('e':'l':'s':'e' : string) = ELSE : scan string

scan ('(' : string) = LPAR : scan string
scan (')' : string) = RPAR : scan string

scan ('+' : string) = PLUS : scan string
scan ('-' : string) = MINUS : scan string
scan ('<' : string) = Token.LT : scan string
scan ('=' : string) = Token.EQ : scan string

scan (' ' : string) = scan string

scan (c : string) =
  if isDigit c then
    let
      (n, s) = num 0 (c : string)
    in
      NUM n : scan s
  else
    let
      (id, s) = Scanner.id "" (c : string)
    in
      ID id : scan s

