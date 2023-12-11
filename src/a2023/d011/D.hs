module D where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Grid2d as G
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data Input = Input {grid :: G.Grid2d Char, emptyCols :: S.Set Int, emptyRows :: S.Set Int}

pInput :: [String] -> Input
pInput str =
  let grid = G.parseFromLines str
      (xR, yR) = G.range grid
      emptyCols = S.fromList $ filter (\x -> all (\y -> G.lookup2dOr '.' (x, y) grid == '.') yR) xR
      emptyRows = S.fromList $ filter (\y -> all (\x -> G.lookup2dOr '.' (x, y) grid == '.') xR) yR
   in Input {..}

expFactor :: Int
expFactor = 1000000 - 1

computeDist :: Input -> ((Int, Int), (Int, Int)) -> Integer
computeDist Input {emptyRows, emptyCols} ((x1, y1), (x2, y2)) =
  let minX = min x1 x2
      maxX = max x1 x2
      minY = min y1 y2
      maxY = max y1 y2
      (_, afterX) = S.split minX emptyCols
      (expX, _) = S.split maxX afterX
      (_, afterY) = S.split minY emptyRows
      (expY, _) = S.split maxY afterY
   in toInteger $ abs (x1 - x2) + abs (y1 - y2) + (length expX + length expY) * expFactor

sol :: Input -> Integer
sol input@Input {grid} =
  let galax = map fst $ filter (\(_, c) -> c == '#') $ G.assocs grid
      pairs = [(g, g2) | (g, i) <- zip galax [0 ..], g2 <- drop (i + 1) galax]
   in sum $ map (computeDist input) pairs

test = sol <$> runSample 11 pInput

run = sol <$> runInput 11 pInput
