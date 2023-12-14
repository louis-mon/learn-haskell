module Grid2d
  ( Grid2d (..),
    range,
    bounds,
    maxYBound,
    lookup2dOr,
    lookup2d,
    parseFromLines,
    showAsGrid,
    pContour,
    lookup2ds,
    elems,
    assocs,
    keys,
    findIndex,
  )
where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

-- Map first indexed by x, then y coord
newtype Grid2d a = Grid2d {getMap :: M.Map Int (M.Map Int a)} deriving (Show, Eq)

bounds :: Grid2d a -> (Int, Int, Int, Int)
bounds (Grid2d g) =
  let xMin = fst $ M.findMin g
      xMax = fst $ M.findMax g
      yMax = L.maximum $ map (fst . M.findMax) $ M.elems g
      yMin = L.minimum $ map (fst . M.findMin) $ M.elems g
   in (xMin, yMin, xMax, yMax)

maxYBound :: Grid2d a -> Int
maxYBound g = let (_, _, _, yMax) = bounds g in yMax

range :: Grid2d a -> ([Int], [Int])
range g =
  let (xMin, yMin, xMax, yMax) = bounds g
   in ([xMin .. xMax], [yMin .. yMax])

parseFromLines :: [String] -> Grid2d Char
parseFromLines ls = Grid2d $ M.fromList $ zip [0 ..] $ map (M.fromList . zip [0 ..]) $ L.transpose ls

showAsGrid :: Grid2d Char -> String
showAsGrid g =
  let (xRange, yRange) = range g
   in unlines $ map (\y -> Maybe.mapMaybe (\x -> lookup2d (x, y) g) xRange) yRange

lookup2dOr :: a -> (Int, Int) -> Grid2d a -> a
lookup2dOr def p g = Maybe.fromMaybe def (lookup2d p g)

lookup2d :: (Int, Int) -> Grid2d a -> Maybe a
lookup2d (x, y) (Grid2d g) = M.lookup x g >>= M.lookup y

pContour :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pContour (x1, y1) (x2, y2) =
  let top = [(x, y1 - 1) | x <- [(x1 - 1) .. x2]]
      bottom = [(x, y2) | x <- [(x1 - 1) .. x2]]
      right = [(x2, y) | y <- [y1 .. (y2 - 1)]]
      left = [(x1 - 1, y) | y <- [y1 .. (y2 - 1)]]
   in top ++ bottom ++ right ++ left

lookup2ds :: [(Int, Int)] -> Grid2d a -> [a]
lookup2ds pts g = concatMap (\p -> Maybe.maybeToList $ lookup2d p g) pts

elems :: Grid2d a -> [a]
elems m = concatMap M.elems $ M.elems $ getMap m

keys :: Grid2d a -> [(Int, Int)]
keys m = map fst $ assocs m

assocs :: Grid2d a -> [((Int, Int), a)]
assocs m = concatMap (\(x, sub) -> map (\(y, v) -> ((x, y), v)) $ M.assocs sub) $ M.assocs $ getMap m

findIndex :: (a -> Bool) -> Grid2d a -> Maybe (Int, Int)
findIndex p m = fst <$> L.find (p . snd) (assocs m)
