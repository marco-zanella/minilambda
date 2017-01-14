module Interpreter (
  run
) where

import Scanner
import Parser
import AST
import Semantics


-- |Runs a piece of code
run :: String -> AST
run code =
  let
    (ast, _) = parse . scan $ code
  in
    reduce ast
