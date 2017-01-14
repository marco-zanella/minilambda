module Interpreter (
  run
) where

import Scanner
import Parser
import AST
import Semantics

run :: String -> AST
run code =
  let
    (ast, _) = parse . scan $ code
  in
    reduce ast



sample = "(fn x . if true then x else 8 ) 42"

sum = "fn x . fn y . (x + y)"
