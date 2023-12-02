module D where

import Lib
import Geo
import Graph

import Data.Char
import Data.Maybe
import qualified Data.Map as M


type Heights = M.Map Point Int
data Problem = Problem {heights::Heights,start::Point, end::Point} deriving (Show)

newtype PGraph = PGraph {getHeights::Heights}

instance Graph PGraph Point where
  neighs (PGraph h) a = [np | np <- map (plus a) way4, nv <- maybeToList $ M.lookup np h, nv <= 1 + fromJust (M.lookup a h)]

parseInput :: [String] -> Problem
parseInput ls = let
  start = fromTuple $ fromJust $ findIndex2d (=='S') ls
  end = fromTuple $ fromJust $ findIndex2d (=='E') ls
  grid = map2d mapHeight ls
  heights = M.fromList $ concatMap (\(j, l) -> zipWith (\i v -> (Point i j, v)) [0..] l) $ zip [0..] grid
  mapHeight x = case x of
    'S' -> 0
    'E' -> 25
    _ -> ord x - ord 'a'
  in Problem{..}

paths Problem{heights, start, end} = let
  path = shortestPath (M.keys $ M.filter (==0) heights) PGraph{getHeights=heights}
  in fromJust $ M.lookup end path


test = runSample 12 (paths . parseInput)
exec = runInput 12 (paths . parseInput)