module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Debug.Trace

pInput :: Parser ([Int], [Int])
pInput = (,) <$> (pKeyword "Time:" *> many pInt) <*> (pKeyword "Distance:" *> many pInt)

solveEquation :: Int -> Int -> Int
solveEquation iT iD = let
  t = fromIntegral iT::Double; d = fromIntegral iD::Double
  delta = t*t - 4*d
  minV = (t - sqrt delta) / 2
  maxV = (t + sqrt delta) / 2
  fminV = ceiling minV
  fmaxV = floor maxV
  in trace (show delta ++ " " ++ show minV ++ " " ++ show maxV) $ (fmaxV - fminV) + 1 - (if fromIntegral fminV == minV then 1 else 0) - (if fromIntegral fmaxV == maxV then 1 else 0)
  
sol1 :: ([Int], [Int]) -> Int
sol1 (t, d) = product $ zipWith solveEquation t d

sol2 :: ([Int], [Int]) -> Int
sol2 (t, d) = let
  tTot = (read $ concatMap show t)::Int
  dTot = (read $ concatMap show d)::Int
  in solveEquation tTot dTot

test = sol2 . parseOrError pInput <$> readInput 6