module Tree where

import Data.Char (isSpace)
import Data.Word (Word8)

newtype Tree = Tree [Tree] deriving (Show, Eq)

printTree :: Tree -> String
printTree (Tree []) = "t"
printTree (Tree ts) = "(t" ++ concatMap printTree ts ++ ")"

treeToExpr :: Tree -> Expr
treeToExpr (Tree cs) = foldl applyExpr (Prg Leaf) (map treeToExpr cs)

applyExpr :: Expr -> Expr -> Expr
applyExpr (Prg p0) (Prg p1) = applyProgram p0 p1
applyExpr (Prg p0) (App a0 a1) = App (Prg p0) (App a0 a1)
applyExpr (App a0 a1) (Prg p0) = App (App a0 a1) (Prg p0)
applyExpr (App a0 a1) (App a2 a3) = App (App (App a0 a1) a2) a3

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

data Comb = L | S | F | A deriving (Show, Enum, Eq)

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
    go _ expr "" = (expr, "")
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

----------
-- TEST --
----------

-- Check if printing a parsed tree yields the same result as parsing and
-- printing *that* tree
-- (i.e. print(parse(text)) == print(parse(print(parse(text)))))
-- The original text is not checked against, as the parentheses can mess it up
-- and it is hard to get it right
parseTest :: String -> Bool
parseTest text
    | text == "" = True
    | all isSpace text = True
    | otherwise =
        let tree1 = parse text
            tree2 = tree1 >>= parse . printTree
            countNodes "" = 0 :: Int
            countNodes (c:cs)
                | c == '(' || c == ')' || isSpace c = countNodes cs
                | otherwise = 1 + countNodes cs
        in
            case (tree1, tree2) of
                (Just t1, Just t2) -> printTree t1 == printTree t2
                _ -> countNodes text == 0
