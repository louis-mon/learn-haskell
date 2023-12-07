module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Char
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

pInput :: Parser [(String, Int)]
pInput = many ((,) <$> (many alphaNumChar) <* space <*> pInt)

handType :: String -> Int
handType cards = let
  byC = M.fromListWith (+) (zip cards (L.repeat 1))
  in case M.size byC of
    5 -> 1
    4 -> 2
    3 -> case L.sort$  M.elems byC of
      [1, 2, 2] -> 3
      [1, 1, 3] -> 4
    2 -> case L.sort $ M.elems byC of
      [2, 3] -> 5
      [1, 4] -> 6
    1 -> 7

valueCard :: Char -> Int
valueCard 'T' = 10
valueCard 'J' = 11
valueCard 'Q' = 12
valueCard 'K' = 13
valueCard 'A' = 14
valueCard x = digitToInt x

compareCards :: (String, Int) -> (String, Int) -> Ordering
compareCards (c1, _) (c2, _) = (handType c1 `compare` handType c2)
  `mappend` (map valueCard c1 `compare` map valueCard c2)
  
handType2 :: String -> Int
handType2 cards = let
  noJ = filter (/= 'J') cards
  byC = M.fromListWith (+) (zip noJ (L.repeat 1))
  in case M.size byC of
    5 -> 1
    4 -> 2
    3 -> case L.sort $ M.elems byC of
      [1, 1, 2] -> 4
      [1, 1, 1] -> 4
      [1, 2, 2] -> 3
      [1, 1, 3] -> 4
    2 -> case L.sort $ M.elems byC of
      [2, 2] -> 5
      [1, 3] -> 6
      [1, 2] -> 6
      [1, 1] -> 6
      [2, 3] -> 5
      [1, 4] -> 6
    1 -> 7
    0 -> 7

valueCard2 :: Char -> Int
valueCard2 'T' = 10
valueCard2 'J' = 1
valueCard2 'Q' = 12
valueCard2 'K' = 13
valueCard2 'A' = 14
valueCard2 x = digitToInt x

compareCards2 :: (String, Int) -> (String, Int) -> Ordering
compareCards2 (c1, _) (c2, _) = (handType2 c1 `compare` handType2 c2)
  `mappend` (map valueCard2 c1 `compare` map valueCard2 c2)
  
sol1 :: String -> Int
sol1 s = let
  input = parseOrError pInput s
  cards = L.sortBy compareCards2 input
  in sum $ zipWith (*) [1..] $ map snd cards

test = sol1 <$> readSample 7
run = sol1 <$> readInput 7