module Parser (
  parse
) where

import Token
import AST


-- |Parses a list of tokens and builds the Abstract Syntax Tree
parse :: [Token] -> (AST, [Token])
parse (Token.UNIT : tokens) = (AST.UNIT, tokens)

parse (Token.ID x : tokens) = (AST.ID x, tokens)

parse (Token.NUM n : tokens) = (AST.NUM n, tokens)

parse (Token.BOOL b : tokens) = (AST.BOOL b, tokens)

parse (Token.FN : tokens) =
  let
    (AST.ID id, Token.DOT : tokens2) = parse tokens
    (m, tokens3) = parse tokens2
  in
    (AST.FN id m, tokens3)



parse (Token.IF : tokens) =
  let
    (m1, Token.THEN : tokens2) = parse tokens
    (m2, Token.ELSE : tokens3) = parse tokens2
    (m3, tokens4) = parse tokens3
  in
    (AST.IF m1 m2 m3, tokens4)


parse (Token.LPAR : tokens) = 
  let
    (m, tokens2) = parse tokens
    (n, tokens3) = parseOp m tokens2
  in
    parseApp n tokens3 


-- |Parses a binary operation
parseOp :: AST -> [Token] -> (AST, [Token])
parseOp m (Token.PLUS : tokens) =
  let
    (n, Token.RPAR : tokens2) = parse tokens
  in
    (AST.SUM m n, tokens2)

parseOp m (Token.MINUS : tokens) = 
  let
    (n, Token.RPAR : tokens2) = parse tokens
  in
    (AST.SUB m n, tokens2)

parseOp m (Token.LT : tokens) = 
  let
    (n, Token.RPAR : tokens2) = parse tokens
  in
    (AST.LT m n, tokens2)

parseOp m (Token.EQ : tokens) = 
  let
    (n, Token.RPAR : tokens2) = parse tokens
  in
    (AST.EQ m n, tokens2)

parseOp m (Token.RPAR : tokens) = (m, tokens)


-- |Parses a function application
parseApp :: AST -> [Token] -> (AST, [Token])
parseApp m (k : tokens) =
  if elem k [Token.THEN, Token.ELSE, Token.RPAR, Token.PLUS, Token.MINUS] then
    (m, k : tokens)
  else
    let
      (n, tokens2) = parse (k : tokens)
    in
      (AST.APP m n, tokens2)
parseApp m [] = (m, [])
