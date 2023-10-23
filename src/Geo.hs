module Geo(Point(..),coords, toPoint, zipCoord, plus, fromTuple, way4) where

data Point = Point {x::Int, y::Int} deriving (Eq, Show, Ord)

coords :: Point -> [Int]
coords (Point x y) = [x, y]

way4 = [Point{x=0,y=1}, Point{x=0,y= -1}, Point{x = 1, y = 0}, Point{x = -1, y = 0}]

fromTuple :: (Int ,Int) -> Point
fromTuple = uncurry Point

toPoint :: [Int] -> Point
toPoint [a, b] = Point a b

zipCoord :: (Int -> Int -> a) -> Point -> Point -> [a]
zipCoord f a b = zipWith f (coords a) (coords b)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)
