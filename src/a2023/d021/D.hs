module D where

import Control.Monad.State
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Geo
import qualified Grid2d as Grid
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data St = St {grid :: Grid.Grid2d Char, iter :: Integer, visited :: S.Set Point, cnt :: Integer}

isPlot :: Point -> Grid.Grid2d Char -> Bool
isPlot Point {x, y} grid =
  let (xMax, yMax) = Grid.size grid
   in maybe False (`elem` ".S") $ Grid.lookup2d ((xMax + x) `mod` xMax, (yMax + y) `mod` yMax) grid

nFinalIter = 65 + 131 * 6

--nFinalIter = 5

diffs :: [Int] -> [[Int]]
diffs = take 20 . iterate (map (\[x, y] -> x - y) . divvy 2 1)

countPos :: S.Set Point -> State St Integer
countPos current = do
  st@St {grid, iter, visited, cnt} <- get
  if iter > nFinalIter
    then return cnt
    else do
      let allNextPos = concatMap (\p -> map (plus p) way4) $ S.toList current
          newNextPos = S.fromList $ filter (not . (`S.member` visited)) $ filter (`isPlot` grid) allNextPos
          newCnt = if odd iter then cnt + toInteger (length newNextPos) else cnt
      put st {iter = iter + 1, visited = S.union visited newNextPos, cnt = newCnt}
      countPos newNextPos

sol :: [String] -> Integer
sol str =
  let grid = Grid.parseFromLines str
      start = fromTuple $ fst $ head $ filter ((== 'S') . snd) $ Grid.assocs grid
   in evalState (countPos (S.singleton start)) St {grid, iter = 1, visited = S.singleton start, cnt = 0}

test = runSample 21 sol

run = runInput 21 sol

solve :: Integer -> Integer
solve i = 3648 + (i * 29208 + i * i * 58116)

{-
k0=3648,
k0+k1+k2=90972,
k0+2k1+4k2=294528,
k0+3k1+9k2=614316,
k0+4k1+16k2=1033973

k0=26,
k0+k1+k2=588,
k0+2k1+4k2=1878,
k0+3k1+9k2=3896,
k0+4k1+16k2=6642

i=101150
-}
