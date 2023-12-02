module GenTree(genTree) where

import Lib
import System.Directory
import Data.Foldable

year :: String
year = "2023"

libs :: [String]
libs = [
  "Lib",
  "qualified Data.Map as M",
  "qualified Data.List as L",
  "qualified Data.Set as S",
  "Text.Megaparsec hiding (State)",
  "Text.Megaparsec.Char",
  "qualified Text.Megaparsec.Char.Lexer as Lex"
  ]

genDay :: Int -> IO ()
genDay di = do
  let dirPath = "src/a" ++ year ++ "/d" ++ padZeros di
  createDirectoryIfMissing True dirPath
  writeFile (dirPath ++ "/D.hs") ("module D where\n\n" ++ unlines (map ("import " ++) libs) ++ "\n")
  writeFile (dirPath ++ "/sample.txt") ""

genTree :: IO ()
genTree = do
  let days = [3..25]
  traverse_ genDay days