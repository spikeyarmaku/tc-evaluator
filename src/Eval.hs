module Eval where

import Tree

isNormal :: Expr -> Bool
isNormal (App _ _) = False
isNormal _ = True

eval :: Expr -> Expr
eval expr =
    let (expr', _) = step expr
    in  if isNormal expr' then expr' else eval expr'

data Rule = RuleL | RuleS | Rule1 | Rule2 | Rule3a | Rule3b | Rule3c | NoRule
    deriving (Show)

step :: Expr -> (Expr, [Rule])
-- Leaf -> Stem
step (App (Prg Leaf) (Prg a)) = (Prg (Stem a), [RuleL])
-- Stem -> Fork
step (App (Prg (Stem a)) (Prg b)) = (Prg (Fork a b), [RuleS])
-- ttab = a             -> ALab = a
step (App (Prg (Fork Leaf a)) _) = (Prg a, [Rule1])
-- t(ta)bc = ac(bc)     -> A(Sa)bc = A(Aac)(Abc)
step (App (Prg (Fork (Stem a) b)) c) =
    (applyExpr (applyExpr (Prg a) c) (applyExpr (Prg b) c), [Rule2])
-- t(tab)ct = a         -> A(Fab)cL = a
step (App (Prg (Fork (Fork a _) _)) (Prg Leaf)) = (Prg a, [Rule3a])
-- t(tab)c(tu) = bu     -> A(Fab)c(Su) = Abu
step (App (Prg (Fork (Fork _ b) _)) (Prg (Stem u))) = (applyProgram b u, [Rule3b])
-- t(tab)c(tuv) = cuv   -> A(Fab)c(Fuv) = A(Acu)v
step (App (Prg (Fork (Fork _ _) c)) (Prg (Fork u v))) =
    (applyExpr (applyProgram c u) (Prg v), [Rule3c])
step (App left right) =
    let (left', r0) = step left
        (right', r1) = step right
    in  (App left' right', r0 ++ r1)
step e = (e, [NoRule])
