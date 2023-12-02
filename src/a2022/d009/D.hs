module D where

import Data.List as L
import qualified Data.Set as S
import Lib

data Point = Point {x::Int, y::Int} deriving (Eq, Show, Ord)

type State = [Point]

coords :: Point -> [Int]
coords (Point x y) = [x, y]

toPoint :: [Int] -> Point
toPoint [a, b] = Point a b

zipCoord :: (Int -> Int -> a) -> Point -> Point -> [a]
zipCoord f a b = zipWith f (coords a) (coords b)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

parseCmd :: String -> [Point]
parseCmd line =
  let
    (dir, ns) = L.splitAt 1 line
    n = readInt ns
    p = case dir of
      "R" -> Point{x=1, y=0}
      "U" -> Point{x=0, y=1}
      "D" -> Point{x=0, y= -1}
      "L" -> Point{x= -1, y=0}
    in L.replicate n p

parse :: [String] -> [Point]
parse = concatMap parseCmd

isAdjacent :: Point -> Point -> Bool
isAdjacent a b = all ((<=1) . abs) (zipCoord (-) a b)

computeTail :: Point -> Point -> Point
computeTail h t
  | isAdjacent t h = t
  | otherwise = plus delta t where
    delta = toPoint $ map (\c -> if abs c == 1 then c else c - signum c) $ zipCoord (-) h t

moveState :: State -> Point -> State
moveState (h:rope) p = let
  nHead = plus h p
  in L.scanl' computeTail nHead rope

ropeSize :: Int
ropeSize = 10

moves :: [String] -> [State]
moves = L.scanl' moveState (replicate ropeSize Point{x=0,y=0}) . parse

sol :: [String] -> Int
sol = length . S.fromList . map last . moves

test = testSample 9 36 sol

execute = runInput 9 sol