module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data Card = Card {cardId::Int, winnings::[Int], picked::[Int]} deriving Show

pLine :: Parser Card
pLine = do
  cardId <- pKeyword "Card" *> pInt <* pKeyword ":"
  winnings <- many pInt
  _ <- pKeyword "|"
  picked <- many pInt
  return Card{..}

pProblem :: Parser [Card]
pProblem = many pLine

cntCard :: Card -> Int
cntCard Card{winnings, picked} = let
  inter = L.intersect winnings picked
  in if null inter then 0 else 2^(length inter - 1)

type AccumSt = M.Map Int Int

accumCard :: AccumSt -> Card -> AccumSt
accumCard acc Card{cardId, winnings, picked} = let
  nWin = length $ L.intersect winnings picked
  added = M.fromList (zip [cardId+1..cardId+nWin] $ L.repeat (acc M.! cardId))
  in M.unionWith (+) acc added

sol2 :: String -> Int
sol2 input = let
  cards = parseOrError pProblem input
  initMap = M.fromList (zip [1..length cards] $ L.repeat 1)
  final = L.foldl' accumCard initMap cards
  in sum $ M.elems final

sol1 :: String -> Int
sol1 = sum . map cntCard . parseOrError pProblem

test = sol2 <$> readSample 4
run = sol2 <$> readInput 4