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
    deriving (Show, Eq)

evalWithMainRulesN :: Int -> Expr -> (Expr, [Rule])
evalWithMainRulesN n =
    let rules = [Rule1, Rule2, Rule3a, Rule3b, Rule3c]
    in  fmap (filter (`elem` rules)) . evalWithRulesN n

evalWithMainRules :: Expr -> (Expr, [Rule])
evalWithMainRules =
    let rules = [Rule1, Rule2, Rule3a, Rule3b, Rule3c]
    in  fmap (filter (`elem` rules)) . evalWithRules

evalWithRulesN :: Int -> Expr -> (Expr, [Rule])
evalWithRulesN n e = go n (e, [])
    where
    go :: Int -> (Expr, [Rule]) -> (Expr, [Rule])
    go 0 x = x
    go n (expr, rules) =
        let (expr', rule') = step expr
            allRules = rules ++ [rule']
        in  if isNormal expr'
                then (expr', allRules)
                else go (n - 1) (expr', allRules)

evalWithRules :: Expr -> (Expr, [Rule])
evalWithRules = evalWithRulesN (-1)

showEvalSteps :: Expr -> [(Rule, Expr)]
showEvalSteps e =
    if isNormal e
        then [(NoRule, e)]
        else
            let (e', r) = step e
            in  (r, e') : showEvalSteps e'

step :: Expr -> (Expr, Rule)
-- Leaf -> Stem
step (App (Prg Leaf) (Prg a)) = (Prg (Stem a), RuleL)
-- Stem -> Fork
step (App (Prg (Stem a)) (Prg b)) = (Prg (Fork a b), RuleS)
-- ttab = a             -> ALab = a
step (App (Prg (Fork Leaf a)) _) = (Prg a, Rule1)
-- t(ta)bc = ac(bc)     -> A(Sa)bc = A(Aac)(Abc)
step (App (Prg (Fork (Stem a) b)) c) =
    (applyExpr (applyExpr (Prg a) c) (applyExpr (Prg b) c), Rule2)
-- t(tab)ct = a         -> A(Fab)cL = a
step (App (Prg (Fork (Fork a _) _)) (Prg Leaf)) = (Prg a, Rule3a)
-- t(tab)c(tu) = bu     -> A(Fab)c(Su) = Abu
step (App (Prg (Fork (Fork _ b) _)) (Prg (Stem u))) =
    (applyProgram b u, Rule3b)
-- t(tab)c(tuv) = cuv   -> A(Fab)c(Fuv) = A(Acu)v
step (App (Prg (Fork (Fork _ _) c)) (Prg (Fork u v))) =
    (applyExpr (applyProgram c u) (Prg v), Rule3c)
step (App left right@(App _ _)) =
    let (right', rule) = step right
    in  (App left right', rule)
step (App left@(App _ _) right) =
    let (left', rule) = step left
    in  (App left' right, rule)
step e = (e, NoRule)
