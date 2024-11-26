module Main where

import Tree
import System.Environment

helpMessage :: String -> String
helpMessage progName =
    "USAGE: " ++ progName ++ " input_file [-cX [output_file]] [-h]\n\n" ++
    "FLAGS:\n" ++
    "-cX output_file     Compile using backend X. Possible values for X:\n" ++
    "                    I - interaction net backend\n" ++
    "                    B - bitcoded tree backend\n" ++
    "-h                  Display this message\n"

main :: IO ()
main = do
    args <- getArgs
    undefined


