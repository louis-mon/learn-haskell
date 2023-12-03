module Grid2d(Grid2d(..), range, lookup2dOr, lookup2d, parseFromLines, pContour, lookup2ds) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as Maybe

newtype Grid2d a = Grid2d {getMap :: M.Map Int (M.Map Int a)} deriving (Show, Eq)

range :: Grid2d a -> ([Int], [Int])
range (Grid2d g) = let
  xMin = fst $ M.findMin g
  xMax = fst $ M.findMax g
  yMax = L.maximum $ map (fst . M.findMax) $ M.elems g
  in ([xMin..xMax],[0..yMax])

parseFromLines :: [String] -> Grid2d Char
parseFromLines ls = Grid2d $ M.fromList $ zip [0..] $ map (M.fromList . zip [0..]) $ L.transpose ls
  
lookup2dOr :: a -> (Int, Int) -> Grid2d a -> a
lookup2dOr def p g = Maybe.fromMaybe def (lookup2d p g)

lookup2d :: (Int, Int) -> Grid2d a -> Maybe a
lookup2d (x, y) (Grid2d g) = M.lookup x g >>= M.lookup y

pContour :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pContour (x1, y1) (x2, y2) = let
  top = [(x, y1 - 1) | x <- [(x1 - 1)..x2]]
  bottom = [(x, y2) | x <- [(x1 - 1)..x2]]
  right = [(x2, y) | y <- [y1..(y2 - 1)]]
  left = [(x1 - 1, y) | y <- [y1..(y2 - 1)]]
  in top++bottom++right++left

lookup2ds :: [(Int, Int)] -> Grid2d a -> [a]
lookup2ds pts g = concatMap (\p -> Maybe.maybeToList $ lookup2d p g) pts
