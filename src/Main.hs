module Main where

import System.Environment (getArgs, getProgName)
import System.IO (hGetContents, openFile, IOMode (..), hClose)
import Control.Monad (unless)

import Tree (parse, treeToExpr, printExprAsTree, Tree, Expr(..),
    printExprWithParens, treeToBinTree, BinTree (Node, BinApp))
import Eval (isNormal, step, eval)
import Inet (compileInet)
import Global (binTreeBase)
import Data.List (intercalate)

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

withTree :: FilePath -> (Tree -> a) -> (a -> IO ()) -> IO ()
withTree fp convert action = do
    h <- openFile fp ReadMode
    f <- hGetContents h
    case parse f of
        Nothing -> return ()
        Just tree -> action (convert tree)
    hClose h

main :: IO ()
main = do
    getArgs >>= print
    getArgs >>= (print . parseFlags)
    config <- fmap (checkConfig . parseFlags) getArgs
    prgName <- getProgName
    case command config of
        DisplayHelp ->  putStrLn $ helpMessage prgName
        Interpret -> withTree (inputFile config) treeToExpr interpret
        Compile InteractionNet -> withTree (inputFile config) treeToBinTree $
            compileInet (outputFile config)
        Compile BinTree -> withTree (inputFile config) treeToBinTree $
            compileBinTree (outputFile config)

-- A ByteOffset is a pointer into the tree data, and an AppOffset is a pointer
-- into the app stack
data Offset = ByteOffset Int | AppOffset Int deriving (Show)

compileBinTree :: FilePath -> BinTree -> IO ()
compileBinTree outFile binTree = do
    let toStr Nothing = "NULL"
        toStr (Just x) = "s" ++ show x
        makeApp :: Maybe Int -> Maybe Int -> Maybe Int -> String
        makeApp n left right =
            "struct Node* " ++ toStr n ++ " = add_node(" ++ toStr left ++
            ", " ++ toStr right ++ ");"
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
            "struct Node* init_program() {\n" ++ toCode binTree ++ "\n}"
    baseStr <- readFile binTreeBase
    writeFile outFile $ baseStr ++ resultApp

{-
[A,A,L,A,F,L,L,L,L]

go (App (A,L,A,F,L,L,L) L) ([], [])
  go (App L (A,F,L,L,L)) ([], [])
    (bytes', offsets') = ([0], [])
    go (App (F,L,L) L) ([0], [])
      (bytes', offsets') = ([0200], [])
      (bytes'', offsets'') = ([02000], [])
      return ([02000], [b1 b4])
    (bytes'', offsets'') = ([02000], [b1 b4])
    return ([02000], [b1 b4, b0 a1])
  (bytes', offsets') = ([02000], [b1 b4, b0 a1])
  (bytes'', offset'') = ([020000], [b1 b4, b0 a1])
  return ([020000], [b1 b4, b0 a1, a2 b5])
-}