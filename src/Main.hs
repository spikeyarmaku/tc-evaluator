module Main where

import System.Environment (getArgs, getProgName)
import System.IO (hGetContents, openFile, IOMode (..), hClose)
import Control.Monad (unless)
import Data.List (intercalate)
import Data.Word (Word8)

import Tree (parse, treeToExpr, printExprAsTree, Tree, Expr(..),
    printExprWithParens, exprToBytes, treeToBinTree)
import Eval (isNormal, step, eval)
import Inet (compileInet)
import Global (binTreeBase)

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
        Compile BinTree-> withTree (inputFile config) treeToExpr $
            compileBinTree (outputFile config)

-- A ByteOffset is a pointer into the tree data, and an AppOffset is a pointer
-- into the app stack
data Offset = ByteOffset Int | AppOffset Int deriving (Show)

-- TODO
-- uint8_t tree[] = { ... };
-- void store() {store_app(tree, tree + 3); ...}
-- When there is an application, it is stored, then its right sub-apps, then its
-- left sub-apps. In this way, the last app is the first to be processed.
-- E.g. ALAFLLL -> [A0]L[A1]FLLL
compileBinTree :: FilePath -> Expr -> IO ()
compileBinTree outFile expr = do
    let go :: Expr -> ([Word8], [(Offset, Offset)]) -> ([Word8], [(Offset, Offset)])
        go (Prg p) (bytes, offsets) = (bytes ++ exprToBytes (Prg p), offsets)
        go (App a0 a1) (bytes, offsets) =
            let (bytes', offsets') = go a1 (bytes, offsets)
                (bytes'', offsets'') = go a0 (bytes', offsets')
                getOffset :: ([Word8], [(Offset, Offset)]) -> Expr -> Offset
                getOffset (bs, _) (Prg _) = ByteOffset (length bs)
                getOffset (_, os) (App _ _) = AppOffset (length os)
                offset = (getOffset (bytes, offsets'') a0, getOffset (bytes', offsets'') a1)
            in  (bytes'', offset : offsets'')
        makeApp :: Int -> Int -> String -> String -> String
        makeApp a b t0 t1 = intercalate ", " ["app(" ++ show a, show b, t0, t1 ++ ");\n"]
        offsetToStr :: (Offset, Offset) -> String
        offsetToStr (ByteOffset n, ByteOffset m) = makeApp n m "PRG" "PRG"
        offsetToStr (ByteOffset n, AppOffset m) = makeApp n m "PRG" "APP"
        offsetToStr (AppOffset n, ByteOffset m) = makeApp n m "APP" "PRG"
        offsetToStr (AppOffset n, AppOffset m) = makeApp n m "APP" "APP"
        (finalBytes, finalOffsets) = go expr ([], [])
        resultArr = "uint8_t tree[] = {" ++ intercalate "," (map show finalBytes) ++ "};"
        resultApp = "void init_program() {\n" ++ concatMap offsetToStr finalOffsets ++ "}"
    baseStr <- readFile binTreeBase
    writeFile outFile $ baseStr ++ resultArr ++ "\n\n" ++ resultApp

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