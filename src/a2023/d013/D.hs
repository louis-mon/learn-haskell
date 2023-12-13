module D where

import qualified Data.List as L
import qualified Data.List.Split as Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

isMirror :: Eq a => [a] -> [a] -> Bool
isMirror x y =
  let l = min (length x) (length y)
   in l > 0 && take l x == take l y

isMirrorSmudge :: Eq a => [[a]] -> [[a]] -> Bool
isMirrorSmudge x y = (== 1) . sum $ zipWith (\lx ly -> length $ filter id $ zipWith (/=) lx ly) x y

findPalindromeIdx :: Eq a => [[a]] -> Maybe Int
findPalindromeIdx l = L.findIndex (uncurry isMirrorSmudge) $ zip (map reverse $ L.inits l) (L.tails l)

findMirror :: [String] -> Int
findMirror ls = case findPalindromeIdx ls of
  Just x -> 100 * x
  Nothing -> fromJust $ findPalindromeIdx $ L.transpose ls

sol :: [String] -> Int
sol = sum . map findMirror . splitOnElem ""

test = runSample 13 sol

run = runInput 13 sol
