module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Geo
import Control.Monad.State
import Data.Maybe
import Debug.Trace

type Input = M.Map Point Char

dirMap = M.fromList [('>', left), ('v', up)]

ptState :: Point -> Char -> Maybe (Bool)
ptState _ '.' = Just True
ptState _ '#' = Nothing
ptState d c = let
  od = dirMap M.! c
  in Just $ od /= d

neights :: Input -> Point -> [(Point,Bool)]
neights input p = mapMaybe ((\(d,np) -> (np,) <$> (M.lookup np input >>= ptState d)) . (\d -> (d, plus p d))) way4

{--
data FillState = FillState{fillVisited::S.Set Point,fillInput::Input, cnt::Int}

fillGraph :: S.Set Point -> State FillState Int
fillGraph s | S.null s = do
  gets $ subtract 1 . cnt
fillGraph current = do
  next <- concat <$> forM (S.toList current) (\c -> do
    FillState{fillVisited, fillInput,cnt=n} <- get
    let cnext = neights fillInput c
    if not . any (\(p,free) -> S.notMember p fillVisited && not free) $ cnext then do
      modify (\s -> s{fillVisited = S.insert c fillVisited})
      return $ map fst . filter snd $ filter ((`S.notMember` fillVisited) . fst) cnext
    else
      return []
    )
  modify (\s -> s{cnt=cnt s + 1})
  fillGraph (S.fromList next)
  --}


neights2 :: Input -> Point -> [Point]
neights2 input p = filter (\n -> '#' /= M.findWithDefault '#' n input) $ map  (plus p) [down, up, left, right]

data LongestSt = LongestSt{lTrimmed::Trimmed, cnt::Int,longestIter::Int}

longestPath :: Point -> Point -> S.Set Point -> Int -> State LongestSt ()
longestPath dest p visited l = do
  LongestSt{lTrimmed} <- get
  let nexts = filter ((`S.notMember` visited) . fst) $ S.toList $ lTrimmed M.! p
  let hasDest = L.find ((==dest) . fst) nexts
  case hasDest of
    Just (_,l2) -> do
      let finalL = l+l2
      s <- get
      modify (\st -> st{cnt=max (cnt st) finalL, longestIter=longestIter st + 1})
      (if cnt s < finalL then trace (show finalL) else id) $ return ()
    Nothing -> do
      if null nexts then
        return ()
      else
        maximum <$> forM nexts (\(n,d) -> longestPath dest n (S.insert p visited) (l+d))

type Trimmed = M.Map Point (S.Set (Point,Int))

data TrimSt = TrimSt{trimVisited::S.Set Point, trimmed::Trimmed,trimInput::Input}

trimGraph :: (Point,Point, Int) -> State TrimSt Trimmed
trimGraph (p,prev,l) = do
  TrimSt{trimVisited, trimInput} <- get
  let baseN = neights2 trimInput p
  let isCross x = length (neights2 trimInput x) /= 2 && x /= prev
  forM_ baseN (\x -> do
      when (isCross x) do
        modify (\s -> s{trimmed = M.unionWith S.union (trimmed s) (M.fromList [(x, S.singleton (prev, l+1)), (prev, S.singleton (x, l+1))])})
    )
  let nexts = filter (`S.notMember` trimVisited) baseN
  let nset = map (\n -> if isCross p then (n,p,1) else (n, prev, l+1)) nexts
  modify (\s -> s{trimVisited=S.insert p trimVisited})
  forM_ nset trimGraph
  gets trimmed

parseGrid :: String -> Input
parseGrid str = let
  ls = lines str
  in M.fromList [(Point{x,y},c) | (row,y) <- zip ls [0..], (c,x) <- zip row [0..]]

sol :: Point -> String -> (Int,Int)
sol dest str = let
  input = parseGrid str
  tr = evalState (trimGraph (Point{x=1,y=0}, Point{x=1,y=0}, 0)) TrimSt{trimVisited=S.empty,trimmed=M.empty,trimInput=input}
  db1 = trace (show (length tr))
  db2 = trace (show (map length (M.elems tr)))
  LongestSt{longestIter, cnt} = db2 $ execState (longestPath dest Point{x=1,y=0} S.empty 0) LongestSt{lTrimmed=tr,cnt=0,longestIter=0}
  in (longestIter, cnt)

test = sol (Point 21 22) <$> readSample 23
run = sol (Point 139 140) <$> readInput 23
