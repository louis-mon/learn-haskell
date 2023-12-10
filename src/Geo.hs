module Geo (Point (..), coords, toPoint, zipCoord, plus, sub, neg, fromTuple, toTuple, way4, left, up, right, down) where

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show, Ord)

coords :: Point -> [Int]
coords (Point x y) = [x, y]

left :: Point
left = Point (-1) 0

right :: Point
right = Point 1 0

up :: Point
up = Point 0 (-1)

down :: Point
down = Point 0 1

way4 :: [Point]
way4 = [Point {x = 0, y = 1}, Point {x = 0, y = -1}, Point {x = 1, y = 0}, Point {x = -1, y = 0}]

fromTuple :: (Int, Int) -> Point
fromTuple = uncurry Point

toTuple :: Point -> (Int, Int)
toTuple (Point x y) = (x, y)

toPoint :: [Int] -> Point
toPoint [a, b] = Point a b

zipCoord :: (Int -> Int -> a) -> Point -> Point -> [a]
zipCoord f a b = zipWith f (coords a) (coords b)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

sub :: Point -> Point -> Point
sub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

neg :: Point -> Point
neg (Point x y) = Point (-x) (-y)
