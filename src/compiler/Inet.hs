module Inet where

import Data.Word (Word8, Word64)

import Tree (BinTree(..), getBinTreeChildren)

data Agent = L | S | F | E | D | A | T | Q | I deriving (Eq, Show)
data PortNum = P0 | P1 | P2 | P3 | P4 | Main deriving (Eq)
data ConnectMode = NoRef | LeftRef | RightRef | FullRef deriving (Eq)

instance Show PortNum where
    show P0 = "P0"
    show P1 = "P1"
    show P2 = "P2"
    show P3 = "P3"
    show P4 = "P4"
    show Main = "PMAIN"

instance Show ConnectMode where
    show NoRef = "NO_REF"
    show LeftRef = "LEFT_REF"
    show RightRef = "RIGHT_REF"
    show FullRef = "FULL_REF"

type RegAddr = Word8
type HeapAddr = Word64 -- architecture-dependent

data Instr
    = MkAgent RegAddr Agent
    | Connect RegAddr PortNum RegAddr PortNum ConnectMode
    | Load RegAddr HeapAddr
    | Return
    deriving (Eq, Show)

type Code = [Instr]

-- Aux types
type Dict a = [(Int, a)]

addEntry :: Int -> a -> Dict a -> Dict a
addEntry k v d = (k, v) : filter ((/= k) . fst) d

getEntry :: Int -> Dict a -> a
getEntry k = snd . head . filter ((== k) . fst)

compileInet :: FilePath -> BinTree -> IO ()
compileInet fp binTree = do
    baseStr <- readFile "src/runtime.c"
    writeFile fp $ baseStr ++ transpile binTree

binTreeToCode :: BinTree -> Code
binTreeToCode t =
    [MkAgent 0 I] ++ fst (compileTree t 1) ++ [Load 0 0, Load 1 1] ++
    [if length (getBinTreeChildren t) < 3
        then Connect 0 P0 1 Main NoRef
        else Connect 1 P1 0 P0 NoRef] ++
    [Return]

compileTree :: BinTree -> HeapAddr -> (Code, HeapAddr)
compileTree t h =
    let children = getBinTreeChildren t
    in  case length children of
        0 -> ([MkAgent 0 L], h + 1)
        1 ->
            let h1 = h + 1
                (code1, h2) = compileTree (head children) h1
                ending =
                    if length (getBinTreeChildren (head children)) < 3
                        then Connect 0 P0 1 Main NoRef
                        else Connect 0 P0 1 P1 NoRef
            in  ([MkAgent 0 S] ++ code1 ++ [Load 0 h, Load 1 h1] ++ [ending], h2)
        2 ->
            let left = head children
                right = children !! 1
                h1 = h + 1
                (code1, h2) = compileTree left h1
                (code2, h3) = compileTree right h2
                ending1 =
                    if length (getBinTreeChildren left) < 3
                        then Connect 0 P0 1 Main NoRef
                        else Connect 0 P0 1 P1 NoRef
                ending2 =
                    if length (getBinTreeChildren right) < 3
                        then Connect 0 P1 2 Main NoRef
                        else Connect 0 P1 2 P1 NoRef
            in  ([MkAgent 0 F] ++ code1 ++ code2 ++
                    [Load 0 h, Load 1 h1, Load 2 h2, ending1, ending2], h3)
        _ ->
            let h1 = h + 1
                BinApp firstChildren lastChild = t
                (code1, h2) = compileTree firstChildren h1
                (code2, h3) = compileTree lastChild h2
                ending1 =
                    if length (getBinTreeChildren firstChildren) < 3
                        then Connect 1 Main 0 Main NoRef
                        else Connect 0 Main 1 P1 NoRef
                ending2 =
                    if length (getBinTreeChildren lastChild) < 3
                        then Connect 0 P0 2 Main NoRef
                        else Connect 0 P0 2 P1 NoRef
            in  ([MkAgent 0 A] ++ code1 ++ code2 ++
                [Load 0 h, Load 1 h1, Load 2 h2, ending1, ending2], h3)

transpile :: BinTree -> String
transpile t =
    let code = binTreeToCode t
        pre = "void init_tree() {\n"
        post = "}\n\nint main() {\n    run();\n}\n"
        indent :: Int -> String -> String
        indent n str = replicate n ' ' ++ str
        codeToString
            :: (Int, Dict String, String) -> Instr -> (Int, Dict String, String)
        codeToString (agentIndex, agents, str) (MkAgent _ a) =
            (agentIndex + 1, agents,
                str ++ indent 4 "struct Agent* agent" ++ show agentIndex ++
                " = mk_agent(AGENT_" ++ show a ++ ");\n")
        codeToString (agentIndex, agents, str) (Connect r0 p0 r1 p1 m) =
            (agentIndex, agents,
                str ++ indent 4 "connect(" ++
                getEntry (fromIntegral r0) agents ++ ", " ++ show p0 ++ ", " ++
                getEntry (fromIntegral r1) agents ++ ", " ++ show p1 ++ ", " ++
                show m ++ ");\n")
        codeToString (agentIndex, agents, str) (Load r h) =
            (agentIndex, addEntry (fromIntegral r) ("agent" ++ show h) agents,
                str)
        codeToString (agentIndex, agents, str) Return = (agentIndex, agents, str)
    in  case foldl codeToString (0, [], pre) code of
            (_, _, codeStr) -> codeStr ++ post
