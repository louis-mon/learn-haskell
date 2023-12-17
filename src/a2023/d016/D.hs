module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Grid2d as Grid
import Geo
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Maybe
import Control.Monad.State
import Debug.Trace

data St = St {visited::S.Set (Point, Point), grid::Grid.Grid2d Char}

getLightDir :: Point -> Char -> [Point]
getLightDir p '.' = [p]
getLightDir Point{x,y} '\\' = [Point y x]
getLightDir Point{x,y} '/' = [Point (-y) (-x)]
getLightDir p@Point{x,y} '|' = case x of
  0 -> [p]
  _ -> [Point y x, Point (-y) (-x)]
getLightDir p@Point{x,y} '-' = case x of
  0 -> [Point y x, Point (-y) (-x)]
  _ -> [p]

disperseLightPoint :: Grid.Grid2d Char -> (Point, Point) -> [(Point, Point)]
disperseLightPoint g (p, dir) = let
  newPos = plus p dir
  in map (newPos,) $ concatMap (getLightDir dir) $ maybeToList $ Grid.lookup2d (toTuple newPos) g

disperseLight :: S.Set (Point, Point) -> State St Int
disperseLight pts = do
  St{visited, grid} <- get
  let newPoints = S.fromList $ concatMap (disperseLightPoint grid) pts
      newVisited = S.union visited newPoints
  if newVisited == visited then
    return $ length (S.map fst visited)
  else do
    put St{visited=newVisited, grid}
    disperseLight (S.difference newPoints visited)

sol :: [String] -> Int
sol str = let
  grid = Grid.parseFromLines str
  (xR, yR) = Grid.range grid
  forPt p = evalState (disperseLight (S.singleton p)) St{grid, visited=S.empty}
  in maximum $ map forPt $ [(Point x 0 , Point 0 1) | x <- xR] ++ [(Point x (last yR) , Point 0 (-1)) | x <- xR] ++
    [(Point 0 y , Point 1 0) | y <- yR] ++ [(Point (last xR) y , Point (-1) 0 ) | y <- yR]

test = runSample 16 sol
run = runInput 16 sol