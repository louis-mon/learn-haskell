module D where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

-- x,y,z
type Point3 = (Int, Int, Int)

-- id,top-left, bottom-right
type Brick = (Int, Point3, Point3)

-- x,y -> zindex,brickid
type HeighMap = M.Map (Int, Int) (Int, Int)

-- associate id -> ids of blocks above
type HasAbove = M.Map Int (S.Set Int)

data St = St {hmap :: HeighMap, result :: [Brick], above :: HasAbove}

bringDown :: Brick -> State St ()
bringDown (bid, (xmin, ymin, zmin), (xmax, ymax, zmax)) = do
  St {hmap, result, above} <- get
  let allPts = [(x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax]]
  let project = map (fromMaybe (0, 0) . (`M.lookup` hmap)) allPts
  let zbelow = maximum (map fst project)
  let nzmin = 1 + zbelow
  let nzmax = nzmin + zmax - zmin
  let np = (bid, (xmin, ymin, nzmin), (xmax, ymax, nzmin + zmax - zmin))
  let nhmap = M.union (M.fromList $ map (,(nzmax, bid)) allPts) hmap
  let covered = M.fromList $ map ((,S.singleton bid) . snd) $ filter (\(z, i) -> z == zbelow) project
  let nHasAbove = M.unionWith S.union covered above
  modify (\s -> s {result = np : result, hmap = nhmap, above = nHasAbove})

downAllBricks :: [Brick] -> State St HasAbove
downAllBricks [] = gets above
downAllBricks (x : xs) = do
  bringDown x
  downAllBricks xs

pBrick :: Parser Brick
pBrick = do
  bid <- unPos . sourceLine <$> getSourcePos
  xmin <- Lex.decimal <* char ','
  ymin <- Lex.decimal <* char ','
  zmin <- Lex.decimal <* char '~'
  xmax <- Lex.decimal <* char ','
  ymax <- Lex.decimal <* char ','
  zmax <- Lex.decimal
  return (bid, (xmin, ymin, zmin), (xmax, ymax, zmax))

-- associate id to number deintegrated
type CountDis = M.Map Int Int

data StDis = StDis {disAbove :: HasAbove, disBelow :: HasAbove}

countDis :: Int -> State StDis (S.Set Int)
countDis bid = do
  StDis {disAbove, disBelow} <- get
  let aboveOf = M.findWithDefault S.empty bid disAbove
  let falling = S.fromList $ filter (\abv -> 1 == length (disBelow M.! abv)) (S.toList aboveOf)
  let nBelow = foldr (M.adjust (S.delete bid)) disBelow (S.toList aboveOf)
  modify (\s -> s {disBelow = nBelow})
  S.union falling . S.unions <$> forM (S.toList falling) countDis

-- compute all stacked bricks and graph relations
computeStack :: String -> ([Brick], HasAbove, HasAbove)
computeStack str =
  let bricks = L.sortOn (\(_, (_, _, z), _) -> z) $ parseOrError (sepEndBy pBrick eol <* eof) str
      above = evalState (downAllBricks bricks) St {result = [], hmap = M.empty, above = M.empty}
      below = M.fromListWith S.union $ concatMap (\(a, b) -> map (,S.singleton a) $ S.toList b) $ M.assocs above
   in (bricks, above, below)

sol :: [Char] -> Int
sol str =
  let (bricks, above, below) = computeStack str
      allAbove = map (\(bid, _, _) -> M.findWithDefault S.empty bid above) bricks
      countSafe = filter (all (\abv -> length (below M.! abv) > 1)) allAbove
   in length countSafe

sol2 :: [Char] -> Int
sol2 str =
  let (bricks, above, below) = computeStack str
      cntFor bid = length $ evalState (countDis bid) StDis {disAbove = above, disBelow = below}
   in sum $ map (cntFor . (\(bid, _, _) -> bid)) bricks

test = sol2 <$> readSample 22

run = sol2 <$> readInput 22