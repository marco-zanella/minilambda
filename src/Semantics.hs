module Semantics (
  step,
  reduce
) where

import AST


step :: AST -> AST
step (SUM (NUM n1) (NUM n2)) = NUM (n1 + n2)
step (SUM (NUM n1) m) = SUM (NUM n1) (step m)
step (SUM m n) = SUM (step m) n

step (SUB (NUM n1) (NUM n2)) = NUM (n1 - n2)
step (SUB (NUM n1) m) = SUB (NUM n1) (step m)
step (SUB m n) = SUB (step m) n

step (IF (BOOL b) m n) = if b then m else n
step (IF m1 m2 m3) = IF (step m1) m2 m3

step (APP (FN x m) v) = if isGround v then sub m x v else APP (FN x m) (step v)
step (APP m n) = APP (step m) n
step m = m



reduce :: AST -> AST
reduce m = if isGround m then m else reduce . step $ m


isGround :: AST -> Bool
isGround UNIT = True
isGround (ID _) = True
isGround (NUM _) = True
isGround (BOOL _) = True
isGround (FN _ _) = True
isGround _ = False
