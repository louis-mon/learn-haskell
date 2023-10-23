module D where

import Lib
import PrefixTree
import Data.List as L

data Cmd = Add String Int | Cd String deriving (Show)

type FS = Tree String Int

parse :: [Cmd] -> String -> [Cmd]
parse cmds cmd
  | "$ cd" `isPrefixOf` cmd = Cd (drop 5 cmd) : cmds
  | "$ ls" == cmd = cmds
  | otherwise = let [t, n] = splitOnElem ' ' cmd
    in Add n (if t == "dir" then 0 else readInt t):cmds
  
parseAll :: [String] -> [Cmd]
parseAll = reverse . L.foldl' parse []

updateTree :: (FS,[String]) -> Cmd -> (FS, [String])
updateTree (tree, path) cmd = case cmd of
  Cd "/" -> (tree, [])
  Cd ".." -> (tree, tail path)
  Cd d -> (tree, d:path)
  Add n s -> (insertPath (reverse path) n s tree, path)
  
buildFs :: [String] -> FS
buildFs = fst . L.foldl' updateTree (make "/" 0, []) . parseAll

sizes :: FS -> (Int, [Int])
sizes (Tree name size children) =
 let subSizes = map sizes children
     own = (size + sum (map fst subSizes))
 in (own, ([own | size == 0])++concatMap snd subSizes)
 
exo1 :: [Int] -> Int
exo1 = sum . filter (<= 100000)

exo2 :: [Int] -> Int
exo2 =  minimum . filter (>= 8381165)
 
execute :: String -> IO Int
execute p = exo2 . snd . sizes . buildFs <$> readFileLines p
