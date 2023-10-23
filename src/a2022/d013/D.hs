module D where

import Lib
import qualified Data.List as L
import Data.Either

import Text.Megaparsec hiding(State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data MList = MValue Int | MList [MList] deriving (Show, Eq)

instance Ord MList where
  MValue a <= MValue b = a <= b
  MList a <= MList b = a <= b
  ma@(MValue _) <= mb@(MList _) = MList [ma] <= mb
  ma@(MList _) <= mb@(MValue _) = ma <= MList [mb]

type Problem = [(MList, MList)]

pList :: Parser MList
pList = choice [MValue <$> Lex.decimal, MList <$> between (char '[') (char ']') (sepBy pList (char ','))]

pProblem :: Parser Problem
pProblem = endBy ((,) <$> pList <* eol <*> pList) (many eol) <* eof

countMatch :: Problem -> Int
countMatch = sum . map fst . filter (uncurry (<) . snd) . zip [1..]

sol1 = countMatch . parseOrError pProblem

special :: [MList]
special = [MList [MList [MValue 2]], MList [MList [MValue 6]]]
sol2 :: String -> Int
sol2 = product . map (+1). L.findIndices (`elem` special) . L.sort . (special++) . concatMap (\(a,b) -> [a, b]) . parseOrError pProblem

test = sol2 <$> readSample 13
exec = sol2 <$> readInput 13