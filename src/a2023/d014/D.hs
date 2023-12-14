module D where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Grid2d as Grid
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Input = Grid.Grid2d Char

newRockPos' :: M.Map Int Char -> Int -> M.Map Int Char
newRockPos' m y = case m M.! y of
  '.' -> m
  '#' -> m
  'O' ->
    let bef = fst $ M.split y m
        upper = case listToMaybe $ dropWhile ((== '.') . snd) $ reverse $ M.assocs bef of
          Just (y', _) -> y' + 1
          Nothing -> 0
     in M.insert upper 'O' $ M.insert y '.' m

moveRocks' :: Input -> Int -> Input
moveRocks' g y = Grid.Grid2d $ M.map (`newRockPos'` y) $ Grid.getMap g

moveMap' :: Input -> Input
moveMap' g =
  let (_, yRange) = Grid.range g
   in L.foldl' moveRocks' g yRange

sol' :: [String] -> Int
sol' str =
  let m = Grid.parseFromLines str
      moved = iterate moveMap' m !! 1
      maxY = Grid.maxYBound m
   in sum $ map (\((_, y), c) -> if c == 'O' then maxY - y + 1 else 0) $ Grid.assocs moved

mapC :: Char -> Char
mapC 'O' = '.'
mapC c = c

moveRocks :: String -> String
moveRocks rocks =
  let groups = L.groupBy (\a b -> mapC a == mapC b) rocks
      move group = case head group of
        '#' -> group
        _ ->
          let nbO = length $ filter (== 'O') group
           in replicate nbO 'O' ++ replicate (length group - nbO) '.'
   in concatMap move groups

type Cache = ([[String]], M.Map [String] Integer)

moveMap :: ([String], Integer) -> State Cache [String]
moveMap (k, i) | i == nbCycle = return k
moveMap (k, i) = do
  (l, m) <- get
  case M.lookup k m of
    Just j ->
      let mCycle = reverse $ take (fromInteger (i - j)) l
       in return $ mCycle !! fromInteger ((nbCycle - i) `mod` toInteger (length mCycle))
    Nothing -> do
      let res = map (reverse . moveRocks) . L.transpose $ k
      put (k : l, M.insert k i m)
      moveMap (res, i + 1)

sol2 :: [String] -> Int
sol2 str =
  let moved = evalState (moveMap (str, 0)) ([], M.empty)
   in sum $ map ((sum . zipWith (\y c -> if c == 'O' then y else 0) [1 ..]) . reverse) (L.transpose moved)

nbCycle :: Integer
nbCycle = 1000000000 * 4

test = runSample 14 sol2

run = runInput 14 sol2
