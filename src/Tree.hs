module Tree where

import Data.Char (isSpace)
import Control.Monad (join)

newtype Tree = Tree [Tree] deriving (Show, Eq)

printTree :: Tree -> String
printTree (Tree []) = "t"
printTree (Tree ts) = "(t" ++ concatMap printTree ts ++ ")"

data Program
    = F Program Program
    | S Program
    | L
    deriving (Show, Eq)

data Expr
    = A Tree Tree
    | P Program
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
