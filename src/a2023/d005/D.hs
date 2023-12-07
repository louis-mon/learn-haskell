module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.List.Split as Split
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

pSeeds :: Parser [Integer]
pSeeds = pKeyword "seeds:" *> many pInteger

data Mapping = Mapping {src::String, dest::String, deps:: [(Integer, Integer, Integer)]} deriving Show

data Problem = Problem {seeds::[Integer], maps:: [Mapping]} deriving Show

pMapping :: Parser Mapping
pMapping = do
  src <- many alphaNumChar
  string "-to-"
  dest <- many alphaNumChar
  string " map:\n"
  deps <- sepEndBy (do
    i1 <- Lex.decimal
    char ' '
    i2 <- Lex.decimal
    char ' '
    i3 <- Lex.decimal
    return (i1, i2, i3)
    ) eol
  return Mapping{src, dest, deps}

pProblem :: Parser Problem
pProblem = do
  seeds <- pSeeds
  maps <- sepEndBy pMapping eol
  return Problem{seeds, maps}

computeMapping :: Integer -> Mapping -> Integer
computeMapping seed Mapping{deps} = case L.find (\(_, s, r) -> seed >= s && seed < s + r) deps of
  Nothing -> seed
  Just (d, s, _) -> d + (seed - s)

locForSeed :: Problem -> Integer -> Integer
locForSeed p seed =
  L.foldl' computeMapping seed (maps p)

sol1 :: String -> Integer
sol1 str = let
  p = parseOrError pProblem str
  in minimum $ map (locForSeed p) (seeds p)

findRanges :: (Integer, Integer) -> [(Integer, Integer, Integer)] -> [(Integer, Integer)]
findRanges (x, l) ranges =
  [(x, min l (s - x)) | let (_, s, _) = head ranges, x < s]
  ++
  [(max (s+r) x, x+l - max (s+r) x) | let (_, s, r) = last ranges, x+l >= s + r]
  ++
  [
    (d + xmin - s, xmax - xmin) |
    (d, s, r) <- ranges,
    let xmin = max s x; xmax = min (s+r) (x+l),
    xmax >= xmin
  ]
  ++
  [
    (xmin, xmax - xmin) |
    [(_, s,r),(_,s2,_)] <- Split.divvy 2 1 ranges,
    let xmin = max (s+r) x; xmax = min s2 (x+l),
    xmax > xmin
  ]

dynComp :: Mapping -> ((Integer, Integer) -> Integer) -> ((Integer, Integer) -> Integer)
dynComp Mapping{deps} f x = minimum $ map f $ findRanges x (L.sortOn (\(_, s, _) -> s) deps)

sol2 :: String -> Integer
sol2 str = let
  p = parseOrError pProblem str
  ranges = map (\[start, l] -> (start,l)) $ Split.chunksOf 2 (seeds p)
  f = foldr dynComp fst (maps p)
  in minimum $ map f ranges

test = sol2 <$> readSample 5
run = sol2 <$> readInput 5