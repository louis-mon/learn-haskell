module D where

import Lib
import Data.Char
import qualified Data.List as L
import qualified Data.Map as M

parseGrid :: [String] -> [[Int]]
parseGrid = map2d digitToInt

traverseLine :: [Int] -> [Bool]
traverseLine = map snd . drop 1 . L.scanl' (\(maxHeight, _) height -> (max maxHeight height, height > maxHeight)) (-1, False)

inTransform :: (forall a. f a -> f a) -> (f b -> f c) -> (f b -> f c)
inTransform tr fct = tr . fct . tr

reverseComp :: ([a] -> [b]) -> ([a] -> [b])
reverseComp = inTransform reverse

mapTranspose :: ([a] -> [b]) -> [[a]] -> [[b]]
mapTranspose f = L.transpose . map f . L.transpose

genGrids :: ([Int] -> [a]) -> (a -> a -> a) -> [[Int]] -> [[a]]
genGrids f comb g = let
   g1 = map f g
   g2 = map (reverseComp f) g
   g3 = mapTranspose f g
   g4 = mapTranspose (reverseComp f) g
   in L.foldl1' (zipWith2d comb) [g1, g2, g3, g4]

sol1 :: [[Int]] -> Int
sol1 = length . filter id . concat . genGrids traverseLine (||)

type IMap = M.Map Int Int

calcView :: [Int] -> [Int]
calcView l = drop 1 $ map fst $ L.scanl' f (0, M.empty) $ zip l [0..] where
  f :: (Int, IMap) -> (Int, Int) -> (Int, IMap)
  f (_, acc) (val, i) =
    case M.lookupGE val acc of
      Just (_, i2) -> (i - i2, M.insert val i greater)
      Nothing -> (i, M.insert val i greater)
    where (_, greater) = M.split val acc
    
calcView2 :: [Int] -> [Int]
calcView2 l = let
  ls = drop 1 $ L.inits l
  view (x:xs) = let (visible, invisible) = L.span (<x) xs in length visible + (length $ take 1 invisible)
  in map (view. reverse) ls

sol2 :: [[Int]] -> Int
sol2 = L.maximum . concat . genGrids calcView2 (*)

test  = testSample "08" 8 (sol2 .parseGrid)

execute :: IO Int
execute = runInput "08" (sol2 . parseGrid)