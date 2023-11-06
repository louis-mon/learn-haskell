module Grid2d(Grid2d(..), range, getAtWithDefault) where

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
  
getAtWithDefault :: a -> (Int, Int) -> Grid2d a -> a
getAtWithDefault def (x, y) (Grid2d g) = Maybe.fromMaybe def (M.lookup x g >>= M.lookup y)
