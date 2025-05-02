module Tree where

import Data.Char (isSpace)
import Data.Word (Word8)

newtype Tree = Tree [Tree] deriving (Eq)

instance Show Tree where
    show (Tree []) = "t"
    show (Tree ts) = "(t" ++ concatMap show ts ++ ")"

-- TC expressions are not allowed as part of Programs, because in the memory
-- structure, applications are parts of a tree that can change, and the rest of
-- the tree should be immutable. If there was an App node where a program is
-- expected, that App node would eventually change the whole subtree, as App
-- nodes are where reductions happen.
data Expr
    = App Expr Expr
    | Prg Program
    deriving (Show, Eq)

data Program
    = Fork Program Program
    | Stem Program
    | Leaf
    deriving (Show, Eq)

data BinTree
    = BinApp BinTree BinTree
    | Node
    deriving (Eq)

instance Show BinTree where
    show Node = "L"
    show (BinApp t0 t1) = "A" ++ show t0 ++ show t1

data Comb = L | S | F | A deriving (Show, Enum, Bounded, Eq)

printExprAsTree :: Expr -> String
printExprAsTree = show . exprToTree

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

printAsComb :: Expr -> String
printAsComb = concatMap show . exprToCombs

exprToCombs :: Expr -> [Comb]
exprToCombs (App e0 e1) = A : (exprToCombs e0 ++ exprToCombs e1)
exprToCombs (Prg Leaf) = [L]
exprToCombs (Prg (Stem p)) = S:exprToCombs (Prg p)
exprToCombs (Prg (Fork p0 p1)) = F:(exprToCombs (Prg p0) ++ exprToCombs (Prg p1))

combToByte :: Comb -> Word8
combToByte = fromIntegral . fromEnum

-- Converts the program parts of an expression into a continuous array of bytes
exprToBytes :: Expr -> [Word8]
exprToBytes = map combToByte . exprToCombs

exprToBytesCompressed :: Expr -> [Word8]
exprToBytesCompressed expr =
    let combs = exprToCombs expr
        go [] = []
        go cs =
            let byte = sum
                     $ zipWith (*) (map fromEnum $ take 4 cs)
                               (map ((4 :: Int) ^) (reverse [0..3] :: [Int]))
            in  fromIntegral byte : go (drop 4 cs)
    in  go combs

printExpr :: Expr -> String
printExpr = concatMap show . exprToCombs

printExprWithParens :: Bool -> Expr -> String
printExprWithParens = go 2
    where
    go level tabbed (App a0 a1) =
        let tabStr = if tabbed then replicate level ' ' else ""
            newlineStr = if tabbed then "\n" else ""
            level' = level + 2
        in  "A" ++ newlineStr ++
                tabStr ++ "(" ++ go level' tabbed a0 ++ ")" ++ newlineStr ++
                tabStr ++ "(" ++ go level' tabbed a1 ++ ")" ++ newlineStr ++
                tabStr
    go _ _ e = concatMap show $ exprToCombs e

getBinTreeChildren :: BinTree -> [BinTree]
getBinTreeChildren Node = []
getBinTreeChildren (BinApp a0 a1) = getBinTreeChildren a0 ++ [a1]

parse :: String -> Maybe Tree
parse = fst . go [] Nothing
    where
    go :: [Maybe Tree] -> Maybe Tree -> String -> (Maybe Tree, String)
    go [] expr "" = (expr, "")
    go _ _ "" = (Nothing, "")
    go stack expr (c:cs)
        | c == ';' = go stack expr (dropWhile (`notElem` "\n\r") cs)
        | c == '(' = go (expr:stack) Nothing cs
        | c == ')' =
            if null stack
                then go stack expr cs
                else go (tail stack) (apply (head stack) expr) cs
        | isSpace c = go stack expr cs
        | otherwise = go stack (apply expr (Just $ Tree [])) cs

treeToBinTree :: Tree -> BinTree
treeToBinTree (Tree []) = Node
treeToBinTree (Tree cs) =
    let newTree = Tree (init cs)
    in  BinApp (treeToBinTree newTree) (treeToBinTree (last cs))

binTreeToTree :: BinTree -> Tree
binTreeToTree Node = Tree []
binTreeToTree (BinApp t1 t2) =
    let Tree children = binTreeToTree t1
    in  Tree (children ++ [binTreeToTree t2])

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
            case parse (show t0) of
                Nothing -> False
                Just t1 -> show t0 == show t1

-- Check if printing a parsed expr is the same as parsing and printing *that*
-- text
printTest :: String -> Bool
printTest text =
    case parse text of
        Nothing -> True
        Just t0 ->
            case parse (printExprAsTree . treeToExpr $ t0) of
                Nothing -> False
                Just t1 -> printExprAsTree (treeToExpr t0) ==
                            printExprAsTree (treeToExpr t1)
