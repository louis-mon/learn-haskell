module D where

import qualified Data.List as L
import qualified Data.List.Split as Split
import qualified Data.Map as M
import qualified Data.Set as S
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

pInput :: Parser [[Int]]
pInput = sepEndBy (sepBy (Lex.signed (return ()) Lex.decimal) hspace) eol <* eof

lineDiff :: [Int] -> [Int]
lineDiff = map (\[a, b] -> b - a) . Split.divvy 2 1

predictLine :: [Int] -> Int
predictLine line =
  let diffs = takeWhile (any (/= 0)) $ iterate lineDiff line
   in foldr (\l e -> head l - e) 0 diffs

sol1 :: [[Int]] -> Int
sol1 = sum . map predictLine

test = sol1 . parseOrError pInput <$> readSample 9

run = sol1 . parseOrError pInput <$> readInput 9
