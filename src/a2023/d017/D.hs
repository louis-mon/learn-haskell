module D where

import Control.Monad.State
import Data.Char
import qualified Data.List as L
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

-- pos, direction, speed
type Node = (Point, Point, Int)

data St = St {minMap :: M.Map Node Int, grid :: Grid.Grid2d Int}

getNextPts :: Node -> [Node]
getNextPts (p, d, s) =
  let front = d
      l = turnLeft d
      r = turnRight d
      dirs = if s < 3 then [front, l, r] else [l, r]
   in map (\d' -> (plus p d', d', if d == d' then s + 1 else 1)) dirs

getNextPts' :: Node -> [Node]
getNextPts' (p, d, s) =
  let front = d
      l = turnLeft d
      r = turnRight d
      dirs
        | s == 0 = [front, l, r]
        | s < 4 = [front]
        | s < 10 = [front, l, r]
        | otherwise = [l, r]
   in map (\d' -> (plus p d', d', if d == d' then s + 1 else 1)) dirs

dijsktra :: S.Set (Int, Node) -> State St Int
dijsktra toVisit = do
  st@St {minMap, grid} <- get
  if null toVisit
    then
      let btm = fromTuple $ Grid.bottomRight grid
       in return $ minimum $ map snd $ filter (\((p, _, s), _) -> p == btm && s >= 4) $ M.assocs minMap
    else do
      let (_, ptToVisit) = S.findMin toVisit
          rest = S.drop 1 toVisit
          currCost = M.findWithDefault 0 ptToVisit minMap
          nextPts = Data.Maybe.mapMaybe (\n@(p, _, _) -> (\cost -> (n, cost + currCost)) <$> Grid.lookup2d (toTuple p) grid) $ getNextPts' ptToVisit
          newPtsToVisit = M.difference (M.fromList nextPts) minMap
          newMinMap = M.unionWith min minMap (M.fromList nextPts)
      put st {minMap = newMinMap}
      let ptWithNewHeat = map (\p -> (newMinMap M.! p, p)) $ M.keys newPtsToVisit
      dijsktra $ S.union rest (S.fromList ptWithNewHeat)

sol :: [String] -> Int
sol str =
  let grid = Grid.parseFromLines $ map (map digitToInt) str
      pInit = (0, (Point 0 0, Point 0 1, 0))
   in evalState (dijsktra (S.singleton pInit)) St {grid, minMap = M.empty}

test = runSample 17 sol

run = runInput 17 sol
