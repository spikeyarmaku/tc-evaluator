module Main where

import System.Environment (getArgs, getProgName)
import System.IO (hGetContents, openFile, IOMode (..), hClose, hSetBuffering,
    stdout, BufferMode (..), hFlush)
import System.Process (readProcess, callCommand)
import Control.Monad (unless, forM_, when)

import Tree (parseTree, treeToExpr, printExprAsTree, Tree, Expr(..),
    printExprWithParens, treeToBinTree, BinTree (Node, BinApp), binTreeToTree,
    size, exprToTree, Program (Fork, Stem, Leaf), exprToCombs)
import Eval (isNormal, step, eval, showEvalSteps, evalWithMainRulesN, Rule(..))
import Inet (compileInet)
import Data.List (intercalate, nub, isInfixOf,)
import System.Exit (exitSuccess, exitFailure)
import Data.Tuple (swap)
import Data.Char (isSpace)

import Debug.Trace (trace)

data InterpretMode = Evaluate | ShowIntermediary deriving (Eq, Show)
data Command = Interpret InterpretMode | Compile Backend | DisplayHelp
    deriving (Eq, Show)
data Backend = InteractionNet | BinTree deriving (Eq, Show)

data Config =
    Config
    { command :: Command
    , inputFile :: String
    , outputFile :: String
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config DisplayHelp "" ""

helpMessage :: String -> String
helpMessage progName = unlines
    [ "USAGE:"
    , progName ++ " -h|--help"
    , progName ++ " input_file -i"
    , progName ++ " input_file -cN [output_file]"
    , ""
    , "FLAGS:"
    , "-i[s]          Use the interpreter to evaluate the input file"
    , "               s - show the applied rules and the intermediary"
    , "                   exporessions"
    , "-cN            Compile with a backend:"
    , "               -ci - interaction net backend"
    , "               -cb - binary tree backend"
    , "-h|--help      Display this message" ]

parseFlags :: [String] -> Config
parseFlags = foldl updateFlag defaultConfig
    where
    updateFlag :: Config -> String -> Config
    updateFlag c f
        | f `elem` ["-h", "--help"] = c {command = DisplayHelp}
        | f == "-ci" = c {command = Compile InteractionNet}
        | f == "-cb" = c {command = Compile BinTree}
        | f == "-is" = c {command = Interpret ShowIntermediary}
        | f == "-i"  = c {command = Interpret Evaluate}
        | otherwise = if inputFile c == ""
                        then c {inputFile = f} else c {outputFile = f}

checkConfig :: Config -> Config
checkConfig c =
    if (command c /= DisplayHelp && inputFile c == "") ||
        (command c == Compile BinTree && outputFile c == "")
        then c {command = DisplayHelp}
        else c

data DebugPrintConfig =
    DebugPrintConfig
        { tabbed :: Bool
        , limitSteps :: Maybe Int
        }

defaultDebugPrintConfig :: DebugPrintConfig
defaultDebugPrintConfig = DebugPrintConfig True Nothing

debugEval :: DebugPrintConfig -> Expr -> IO ()
debugEval (DebugPrintConfig t ls) e = do
    let ls' = fmap (max 0 . (`subtract` 1)) ls
        isLimited = ls == Just 0
    unless isLimited $ putStrLn (printExprWithParens t e)
    if isNormal e
        then return ()
        else
            let (e', rs) = step e
            in  unless isLimited (print rs) >>
                    debugEval (DebugPrintConfig t ls') e'

interpret :: InterpretMode -> Expr -> IO ()
interpret Evaluate e = putStrLn . printExprAsTree . eval $ e
interpret ShowIntermediary e =
    let printPair :: (String, String) -> String
        printPair (str0, str1) = str0 ++ " " ++ str1
    in  putStrLn . intercalate "\n" . (concatMap show (exprToCombs e) :)
    . map   ( printPair . swap . fmap show . swap
            . fmap (concatMap show . exprToCombs)) . showEvalSteps $ e

withTree :: FilePath -> (Tree -> IO ()) -> IO ()
withTree fp action = do
    h <- openFile fp ReadMode
    f <- hGetContents h
    forM_ (parseTree f) action
    hClose h

main :: IO ()
-- main = print $ showEvalPathString "t(t(ttt)tt)(t(tt)t(t(t(ttt)))tt)t"
-- -- Invoke search, with the intent of redirecting the output into a file
-- main = do
--     hSetBuffering stdout LineBuffering
--     search

-- This version compares the runtime's result against the interpreter for small
-- trees
-- main = do
--     forM_ [1400..] $ \i -> do
--         let tree = nthTree i
--         when (i `mod` 100 == 0) $
--             putStrLn $ show i ++ " - " ++ show (size $ binTreeToTree tree)
--         -- TODO run the evaluation, and run the C code, compare results
--         checkProgram tree

main = do
    getArgs >>= print
    getArgs >>= (print . parseFlags)
    config <- fmap (checkConfig . parseFlags) getArgs
    prgName <- getProgName
    case command config of
        DisplayHelp ->  putStrLn $ helpMessage prgName
        Interpret mode -> withTree (inputFile config) $
            interpret mode . treeToExpr
        Compile InteractionNet -> withTree (inputFile config) $
            compileInet (outputFile config) . treeToBinTree
        Compile BinTree -> withTree (inputFile config) $ \t -> do
            compileBinTree (outputFile config) . treeToExpr $ t

checkProgram :: Expr -> IO ()
checkProgram t = do
    let inp_file = "compare.txt"
        outp_file = "compare.c"
        config = Config (Compile BinTree) inp_file outp_file
        strip "" = ""
        strip (c:cs)
            | isSpace c = strip cs
            | otherwise = c : strip cs
    writeFile inp_file (show $ exprToTree t)
    compileBinTree (outputFile config) t
    callCommand "./c_cmp.sh"
    let i_res = printExprAsTree $ eval t
    putStrLn $ show (exprToTree t) ++ " -> " ++  i_res
    readProcess "./compare.out" [] "" >>= \result -> do
        unless (strip result `isInfixOf` i_res) $ do
            putStrLn $ "MISMATCH: " ++ show result
            exitFailure
    hFlush stdout

showTree :: Int -> String
showTree = show . binTreeToTree . nthTree

showEvalPathNum :: Int -> String
showEvalPathNum =
    show . evalWithMainRulesN 20 . treeToExpr . binTreeToTree . nthTree

showEvalPathTree :: Tree -> String
showEvalPathTree =
    show . swap . fmap exprToTree . swap . evalWithMainRulesN 20 . treeToExpr

showEvalPathString :: String -> Maybe String
showEvalPathString = fmap showEvalPathTree . parseTree

-- Find the smallest trees that require at least 3 reduction rules to evaluate
search :: IO ()
search =
    forM_ [1608000..] $ \i -> do
        let tree = nthTree i
        let rs = getRules tree
            isOk = rs == nub rs
        when (i `mod` 1000 == 0) $
            putStrLn $ show i ++ " - " ++ show (size $ binTreeToTree tree)
        when (isOk && length rs == 4) $ do
            putStrLn $ "Found tree with 4 rules: # " ++ show i ++ " - " ++
                show (size $ binTreeToTree tree) ++ " - " ++ show rs
        when (isOk && length rs == 5) $ do
            putStrLn $ "Found matching tree: # " ++ show i ++ " - " ++
                show (size $ binTreeToTree tree) ++ " - " ++ show rs
            exitSuccess

getRules :: BinTree -> [Rule]
getRules = snd . evalWithMainRulesN 7 . treeToExpr . binTreeToTree

generateTree :: Int -> [BinTree]
generateTree 0 = [Node]
generateTree n =
    [BinApp left right
        | i <- [0 .. n - 1]
        , left <- generateTree i
        , right <- generateTree (n - 1 - i)]

nthTree :: Int -> BinTree
nthTree n = concatMap generateTree [0..] !! n

type NodeType = Int
type NodeIndex = Int
data NodeOp = NodeOp NodeType (NodeIndex, NodeIndex) deriving (Show)

-- TODO top node shall be at index 0, the rest can be anywhere in any order
compileBinTree :: FilePath -> Expr -> IO ()
compileBinTree outFile expr = do
    let indent :: Int -> String -> String
        indent n str = replicate n ' ' ++ str
        addNode :: NodeType -> (NodeIndex, NodeIndex) -> String
        addNode nodeType (leftIndex, rightIndex)
            = "tree_add_node(tree, "
            ++ intercalate ", " [["Stem", "Fork", "App"] !! nodeType
                                , show leftIndex, show rightIndex ] ++ ");"
        makeIndices :: Int -> Expr -> Expr -> (Int, (Int, Int))
        makeIndices i (Prg Leaf) (Prg Leaf) = (i - 1, (0, 0))
        makeIndices i (Prg Leaf) _ = (i, (0, i))
        makeIndices i _ (Prg Leaf) = (i, (i, 0))
        makeIndices i _ _ = (i + 1, (i, i + 1))
        -- Return the last variable number used and the code generated so far
        -- compile first_available_index expr -> (index_of_top_node, operations)
        compile :: Int -> [Expr] -> [String]
        compile _ [] = []
        compile c (e:es) = -- trace (show c ++ ": " ++ show (e:es)) $
            case e of
                App e0 e1 ->
                    let (c', is) = makeIndices (c + 1) e0 e1
                    in  addNode 2 is : compile c' (es ++ [e0, e1])
                Prg (Fork p0 p1) ->
                    let (c', is) = makeIndices (c + 1) (Prg p0) (Prg p1)
                    in  addNode 1 is : compile c' (es ++ [Prg p0, Prg p1])
                Prg (Stem p0) ->
                    let (c', is) = makeIndices (c + 1) (Prg p0) (Prg Leaf)
                    in  addNode 0 is : compile c' (es ++ [Prg p0])
                Prg Leaf -> compile c es
        code = map (indent 4) $ compile 0 [expr]
        resultApp
            =  "void init_program(struct Tree* tree) {\n"
            ++ intercalate "\n" code ++ "\n}\n"
    writeFile outFile resultApp
    print expr
    print $ compile 0 [expr]
