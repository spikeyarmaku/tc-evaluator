module Tree where

import Data.Char (isSpace)
import Data.Word (Word8)

newtype Tree = Tree [Tree] deriving (Show, Eq)

printTree :: Tree -> String
printTree (Tree []) = "t"
printTree (Tree ts) = "(t" ++ concatMap printTree ts ++ ")"

printExpr :: Expr -> String
printExpr = printTree . exprToTree

treeToExpr :: Tree -> Expr
treeToExpr (Tree cs) = foldl applyExpr (Prg Leaf) (map treeToExpr cs)

exprToTree :: Expr -> Tree
exprToTree (Prg Leaf) = Tree []
exprToTree (Prg (Stem a)) = Tree [exprToTree (Prg a)]
exprToTree (Prg (Fork a b)) = Tree [exprToTree (Prg a), exprToTree (Prg b)]
exprToTree (App a0 a1) =
    let (Tree tree0) = exprToTree a0
        tree1 = exprToTree a1
    in  Tree (tree0 ++ [tree1])

applyExpr :: Expr -> Expr -> Expr
applyExpr (Prg p0) (Prg p1) = applyProgram p0 p1
applyExpr (Prg p0) (App a0 a1) = App (Prg p0) (App a0 a1)
applyExpr (App a0 a1) (Prg p0) = App (App a0 a1) (Prg p0)
applyExpr (App a0 a1) (App a2 a3) = App (App a0 a1) (App a2 a3)

applyProgram :: Program -> Program -> Expr
applyProgram (Fork p0 p1) p = App (Prg (Fork p0 p1)) (Prg p)
applyProgram (Stem p0) p = Prg (Fork p0 p)
applyProgram Leaf p = Prg (Stem p)

isProgram :: Expr -> Bool
isProgram (Prg _) = True
isProgram _ = False

getProgram :: Expr -> Maybe Program
getProgram (Prg prg) = Just prg
getProgram _ = Nothing

printComb :: Expr -> String
printComb = concatMap show . exprToComb

exprToComb :: Expr -> [Comb]
exprToComb (App e0 e1) = A : (exprToComb e0 ++ exprToComb e1)
exprToComb (Prg Leaf) = [L]
exprToComb (Prg (Stem p)) = S:exprToComb (Prg p)
exprToComb (Prg (Fork p0 p1)) = F:(exprToComb (Prg p0) ++ exprToComb (Prg p1))

data Comb = L | S | F | A deriving (Show, Enum, Bounded, Eq)

exprToBytes :: Expr -> [Word8]
exprToBytes expr =
    let combs = exprToComb expr
        go [] = []
        go cs =
            let byte = sum
                     $ zipWith (*) (map fromEnum $ take 4 cs)
                               (map ((4 :: Int) ^) (reverse [0..3] :: [Int]))
            in  fromIntegral byte : go (drop 4 cs)
    in  go combs

data Program
    = Fork Program Program
    | Stem Program
    | Leaf
    deriving (Show, Eq)

-- Expr's are not allowed as part of Programs, because in the memory structure,
-- applications are parts of a tree that can change, and the rest of the tree
-- should be immutable. If there was an A node where a program is expected, that
-- A node would eventually change the whole subtree, as A nodes are where
-- reductions happen.
data Expr
    = App Expr Expr
    | Prg Program
    deriving (Show, Eq)

parse :: String -> Maybe Tree
parse = fst . go [] Nothing
    where
    go :: [Maybe Tree] -> Maybe Tree -> String -> (Maybe Tree, String)
    go [] expr "" = (expr, "")
    go _ _ "" = (Nothing, "")
    go stack expr (c:cs)
        | c == '(' = go (expr:stack) Nothing cs
        | c == ')' =
            if null stack
                then go stack expr cs
                else go (tail stack) (apply (head stack) expr) cs
        | isSpace c = go stack expr cs
        | otherwise = go stack (apply expr (Just $ Tree [])) cs

apply :: Maybe Tree -> Maybe Tree -> Maybe Tree
apply Nothing Nothing = Nothing
apply Nothing (Just t) = Just t
apply (Just t) Nothing = Just t
apply (Just (Tree t1)) (Just (Tree t2)) = Just $ Tree $ t1 ++ [Tree t2]

size :: Tree -> Int
size (Tree cs) = 1 + sum (map size cs)

-- Count the number of leaves, stems, forks and applications
stats :: [Comb] -> [(Comb, Int)]
stats = go (zip [minBound..] (repeat 0))
    where
    go :: [(Comb, Int)] -> [Comb] -> [(Comb, Int)]
    go s [] = s
    go s (c:cs) =
        let newS = map (\(comb, count) ->
                if comb == c then (comb, count + 1) else (comb, count)) s
        in  go newS cs

----------
-- TEST --
----------

-- Check if printing a parsed tree yields the same result as parsing and
-- printing *that* tree
-- (i.e. print(parse(text)) == print(parse(print(parse(text)))))
-- The original text is not checked against, as the parentheses can mess it up
-- and it is hard to get it right
parseTest :: String -> Bool
parseTest text =
    case parse text of
        Nothing -> True
        Just t0 ->
            case parse (printTree t0) of
                Nothing -> False
                Just t1 -> printTree t0 == printTree t1

-- Check if printing a parsed expr is the same as parsing and printing *that*
-- text
printTest :: String -> Bool
printTest text =
    case parse text of
        Nothing -> True
        Just t0 ->
            case parse (printExpr . treeToExpr $ t0) of
                Nothing -> False
                Just t1 ->
                    printExpr (treeToExpr t0) == printExpr (treeToExpr t1)
