module Main where

import System.Environment (getArgs, getProgName)
import System.IO (hGetContents, openFile, IOMode (..), hClose, hSetBuffering,
    stdout, BufferMode (..), hFlush)
import System.Process (readProcess, callCommand)
import Control.Monad (unless, forM_, when)

import Tree (parse, treeToExpr, printExprAsTree, Tree, Expr(..),
    printExprWithParens, treeToBinTree, BinTree (Node, BinApp), binTreeToTree,
    size, exprToTree)
import Eval (isNormal, step, eval, evalWithRules, evalWithMainRulesN, Rule(..))
import Inet (compileInet)
import Global (binTreeBase)
import Data.List (intercalate, nub, isInfixOf)
import System.Exit (exitSuccess, exitFailure)
import Data.Tuple (swap)
import Data.Char (isSpace)

data Command = Interpret | Compile Backend | DisplayHelp deriving (Eq, Show)
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
    , "-i             Use the interpreter to evaluate the input file"
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
        | f == "-i"  = c {command = Interpret}
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

interpret :: Expr -> IO ()
interpret = putStrLn . printExprAsTree . eval

withTree :: FilePath -> (Tree -> IO ()) -> IO ()
withTree fp action = do
    h <- openFile fp ReadMode
    f <- hGetContents h
    forM_ (parse f) action
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
        Interpret -> withTree (inputFile config) $ interpret . treeToExpr
        Compile InteractionNet -> withTree (inputFile config) $
            compileInet (outputFile config) . treeToBinTree
        Compile BinTree -> withTree (inputFile config) $ \t ->  do
            let tree = treeToBinTree t
            print . showEvalPathTree . binTreeToTree $ tree
            compileBinTree (outputFile config) tree

checkProgram :: BinTree -> IO ()
checkProgram t = do
    let inp_file = "compare.txt"
        outp_file = "compare.c"
        config = Config (Compile BinTree) inp_file outp_file
        strip "" = ""
        strip (c:cs)
            | isSpace c = strip cs
            | otherwise = c : strip cs
    writeFile inp_file (show $ binTreeToTree t)
    compileBinTree (outputFile config) t
    callCommand "./c_cmp.sh"
    let i_res = printExprAsTree $ eval (treeToExpr $ binTreeToTree t)
    putStrLn $ show (binTreeToTree t) ++ " -> " ++  i_res
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
showEvalPathString = fmap showEvalPathTree . parse

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

-- A ByteOffset is a pointer into the tree data, and an AppOffset is a pointer
-- into the app stack
data Offset = ByteOffset Int | AppOffset Int deriving (Show)

compileBinTree :: FilePath -> BinTree -> IO ()
compileBinTree outFile binTree = do
    let toStr Nothing = "NULL"
        toStr (Just x) = "s" ++ show x
        makeApp :: Maybe Int -> Maybe Int -> Maybe Int -> String
        makeApp n left right =
            "struct Node* " ++ toStr n ++ " = add_node(tree, " ++
            toStr left ++ ", " ++ toStr right ++ ");"
        -- Return the last variable number used and thecode generated so far
        compile :: Int -> BinTree -> Maybe (Int, [String])
        compile _ Node = Nothing
        compile n (BinApp left right) =
            case compile n left of
                Nothing ->
                    case compile n right of
                        Nothing -> Just (n, [makeApp (Just n) Nothing Nothing])
                        Just (nRight, codeRight) ->
                            Just (nRight + 1,
                                codeRight ++ [makeApp (Just $ nRight + 1)
                                    Nothing (Just nRight)])
                Just (nLeft, codeLeft) ->
                    case compile (nLeft + 1) right of
                        Nothing ->
                            Just (nLeft + 1,
                                codeLeft ++ [makeApp (Just $ nLeft + 1)
                                    (Just nLeft) Nothing])
                        Just (nRight, codeRight) ->
                            Just (nRight + 1,
                                    codeLeft ++ codeRight ++
                                    [makeApp (Just $ nRight + 1)
                                        (Just nLeft) (Just nRight)])
        indent :: Int -> String -> String
        indent n str = replicate n ' ' ++ str
        toCode t =
            case compile 0 t of
                Nothing -> indent 4 "return NULL;"
                Just (n, code) ->
                    intercalate "\n" $ map (indent 4) $
                        code ++ ["return " ++ toStr (Just n) ++ ";"]
        resultApp =
            "struct Node* init_program(struct Tree* tree) {\n" ++
            toCode binTree ++ "\n}\n"
    baseStr <- readFile binTreeBase
    writeFile outFile $ baseStr ++ resultApp
