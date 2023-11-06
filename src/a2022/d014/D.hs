module D where

import Data.Either
import qualified Data.List as L
import qualified Data.List.Split as Split
import qualified Data.Map as M
import Data.Maybe
import Grid2d as G
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Input = [[(Int, Int)]]

pLine :: Parser [(Int, Int)]
pLine = sepBy ((,) <$> Lex.decimal <*> (char ',' *> Lex.decimal)) (string " -> ")

pProblem :: Parser Input
pProblem = sepBy pLine newline

pointRange :: [(Int, Int)] -> [(Int, Int)]
pointRange [(x1, y1), (x2, y2)]
  | x1 == x2 = map (x1,) [(min y1 y2) .. (max y1 y2)]
  | y1 == y2 = map (,y1) [(min x1 x2) .. (max x1 x2)]

addToState :: (Int, Int) -> Elem -> State -> State
addToState (x, y) e state = mergeStates state $ Grid2d $ M.singleton x $ M.singleton y e

emptyState :: State
emptyState = Grid2d M.empty

buildSection :: [(Int, Int)] -> State
buildSection pts =
  let r = pointRange pts
   in foldr (`addToState` Rock) emptyState r

buildPath :: [(Int, Int)] -> State
buildPath path = foldr mergeStates emptyState $ map buildSection $ Split.divvy 2 1 path

mergeStates :: State -> State -> State
mergeStates (Grid2d g1) (Grid2d g2) = Grid2d $ M.unionWith M.union g1 g2

buildState :: Input -> State
buildState = foldr1 mergeStates . map buildPath

data Elem = Sand | Rock | Empty deriving (Show, Eq)

type State = G.Grid2d Elem

parseInput :: String -> Input
parseInput = parseOrError pProblem

getAt :: (Int, Int) -> State -> Elem
getAt = G.getAtWithDefault Empty

nextState :: State -> State
nextState g =
  let rec :: (Int, Int) -> State -> State
      rec (x, y) st@(Grid2d curr) = case M.lookup x curr >>= M.lookupGT y of
        Just (y1, _) -> case M.lookup (x - 1) curr >>= M.lookup y1 of
          Just _ -> case M.lookup (x + 1) curr >>= M.lookup y1 of
            Just _ -> addToState (x, y1 - 1) Sand st
            Nothing -> rec (x + 1, y1) st
          Nothing -> rec (x -1, y1) st
        Nothing -> st
   in rec (500, 0) g

nextState2 :: (State, Bool, Int) -> (State, Bool, Int)
nextState2 (g, _, yMax) =
  let lk x = Just . M.insert yMax Rock . M.findWithDefault M.empty x
      rec :: (Int, Int) -> State -> (State, Bool)
      rec (x, y) st@(Grid2d curr) = case lk x curr >>= M.lookupGE y of
        Just (0, _) -> (st, True)
        Just (y1, _) -> case lk (x - 1) curr >>= M.lookup y1 of
          Just _ -> case lk (x + 1) curr >>= M.lookup y1 of
            Just _ -> (addToState (x, y1 - 1) Sand st, False)
            Nothing -> rec (x + 1, y1) st
          Nothing -> rec (x -1, y1) st
      (next, stuck) = rec (500, 0) g
   in (next, stuck, yMax)

prepareState2 :: State -> (State, Bool, Int)
prepareState2 g = let (_, yRange) = G.range g in (g, False, last yRange + 2)

showMap :: State -> IO ()
showMap state =
  let (rangeX, rangeY) = G.range state
      mapElem :: Elem -> Char
      mapElem Sand = 'o'
      mapElem Rock = '#'
      mapElem Empty = '.'
      ls = map (\y -> map (\x -> mapElem $ getAt (x, y) state) rangeX) rangeY
   in putStrLn $ unlines ls

genStates :: String -> [State]
genStates = takeWhile2 (/=) . iterate nextState . buildState . parseInput

sol2 :: String -> Int
sol2 = subtract 1 . length . map (\(x, _, _) -> x) . takeWhile (\(_, stuck, _) -> not stuck) .
  iterate nextState2 . prepareState2 . buildState . parseInput

test = sol2 <$> readSample 14

run = sol2 <$> readInput 14
