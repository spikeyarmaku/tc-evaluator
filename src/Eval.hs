module Eval where

import Tree

isNormal :: Expr -> Bool
isNormal (App _ _) = False
isNormal _ = True

eval :: Expr -> Expr
eval expr =
    let expr' = step expr
    in  if isNormal expr' then expr' else eval expr'

step :: Expr -> Expr
-- ttab = a             -> ALab = a
step (App (Prg (Fork Leaf a)) _) = Prg a
-- t(ta)bc = ac(bc)     -> A(Sa)bc = A(Aac)(Abc)
step (App (Prg (Fork (Stem a) b)) c) =
    applyExpr (applyExpr (Prg a) c) (applyExpr (Prg b) c)
-- t(tab)ct = a         -> A(Fab)cL = a
step (App (Prg (Fork (Fork a _) _)) (Prg Leaf)) = Prg a
-- t(tab)c(tu) = bu     -> A(Fab)c(Su) = Abu
step (App (Prg (Fork (Fork _ b) _)) (Prg (Stem u))) = applyProgram b u
-- t(tab)c(tuv) = cuv   -> A(Fab)c(Fuv) = A(Acu)v
step (App (Prg (Fork (Fork _ _) c)) (Prg (Fork u v))) =
    applyExpr (applyProgram c u) (Prg v)
step (App left right) = App (step left) right
step e = e
