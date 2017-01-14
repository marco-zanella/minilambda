module AST (
  AST(..),
  fv,
  sub
) where

import  Data.Set (Set)
import qualified Data.Set as Set


-- |Abstract Syntax Tree
data AST =
    UNIT
  | ID String
  | NUM Int
  | BOOL Bool
  | SUM AST AST
  | SUB AST AST
  | LT AST AST
  | EQ AST AST
  | IF AST AST AST
  | FN String AST
  | APP AST AST
  deriving (Show, Eq)


-- |Set of free variables in an expression
fv :: AST -> Set String
fv UNIT = Set.empty
fv (ID x) = Set.singleton x
fv (NUM _) = Set.empty
fv (BOOL _) = Set.empty
fv (SUM m n) = Set.union (fv m) (fv n)
fv (SUB m n) = Set.union (fv m) (fv n)
fv (AST.LT m n) = Set.union (fv m) (fv n)
fv (AST.EQ m n) = Set.union (fv m) (fv n)
fv (IF m1 m2 m3) = Set.union (fv m1) (Set.union (fv m2) (fv m3))
fv (FN x m) = Set.delete x (fv m)
fv (APP m n) = Set.union (fv m) (fv n)


-- |Syntactic substitution
sub :: AST -> String -> AST -> AST
sub UNIT _ _ = UNIT
sub (ID y) x n = if y == x then n else ID y
sub (NUM n) _ _ = NUM n
sub (BOOL b) _ _ = BOOL b
sub (SUM m1 m2) x n = SUM (sub m1 x n) (sub m2 x n)
sub (SUB m1 m2) x n = SUB (sub m1 x n) (sub m2 x n)
sub (AST.LT m1 m2) x n = AST.LT (sub m1 x n) (sub m2 x n)
sub (AST.EQ m1 m2) x n = AST.EQ (sub m1 x n) (sub m2 x n)
sub (IF m1 m2 m3) x n = IF (sub m1 x n) (sub m2 x n) (sub m3 x n)
sub (FN y m) x n = FN y (if y == x then m else sub m x n)
sub (APP m1 m2) x n = APP (sub m1 x n) (sub m2 x n)
