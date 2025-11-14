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
import Data.List (intercalate, nub, isInfixOf)
import System.Exit (exitSuccess, exitFailure)
import Data.Tuple (swap)
import Data.Char (isSpace)

data Command = Interpret | Compile | DisplayHelp deriving (Eq, Show)

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
    , "-c             Compile to C"
    , "-h|--help      Display this message" ]

parseFlags :: [String] -> Config
parseFlags = foldl updateFlag defaultConfig
    where
    updateFlag :: Config -> String -> Config
    updateFlag c f
        | f `elem` ["-h", "--help"] = c {command = DisplayHelp}
        | f == "-c" = c {command = Compile}
        | f == "-i"  = c {command = Interpret}
        | otherwise = if inputFile c == ""
                        then c {inputFile = f} else c {outputFile = f}

checkConfig :: Config -> Config
checkConfig c =
    if (command c /= DisplayHelp && inputFile c == "") ||
        (command c == Compile && outputFile c == "")
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
main = do
    getArgs >>= print
    getArgs >>= (print . parseFlags)
    config <- fmap (checkConfig . parseFlags) getArgs
    prgName <- getProgName
    case command config of
        DisplayHelp ->  putStrLn $ helpMessage prgName
        Interpret -> withTree (inputFile config) $ interpret . treeToExpr
        Compile -> withTree (inputFile config) $
            compileInet (outputFile config) . treeToBinTree
