module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad.State
import Data.Maybe
import Data.Ratio
import Debug.Trace

type Point3 = (Rational, Rational, Rational)
-- pos, speed
type Hail = (Point3, Point3)

x (px,_,_) = px
y (_,py,_) = py

-- Function to find the intersection point
findIntersection :: Hail -> Hail -> Maybe (Point3)
findIntersection (p1, v1) (p2, v2)
  | crossProduct == 0 || t1 < 0 || t2 < 0 = Nothing  -- Lines are parallel, no intersection
  | otherwise = Just intersectionPoint
  where
    deltaP = (x p2 - x p1, y p2 - y p1,0)
    crossProduct = crossZ v1 v2
    t1 = crossZ deltaP v2 / crossProduct
    t2 = crossZ deltaP v1 / crossProduct
    intersectionPoint = (x p1 + t1 * x v1, y p1 + t1 * y v1,0)

isPointWithinRange :: (Rational,Rational) -> Point3 -> Bool
isPointWithinRange (minC, maxC) (x, y, _) =
  x >= minC && x <= maxC && y >= minC && y < maxC


-- Function to calculate the cross product of 2D vectors
crossZ :: Point3 -> Point3 -> Rational
crossZ (x1, y1,_) (x2, y2,_) = x1 * y2 - x2 * y1

pSigRational :: Parser Rational
pSigRational = toRational <$> pSigInteger

pHail :: Parser Hail
pHail = do
  xmin <- pSigRational <* symbol ","
  ymin <- pSigRational <* symbol ","
  zmin <- pSigRational <* symbol "@"
  xmax <- pSigRational <* symbol ","
  ymax <- pSigRational <* symbol ","
  zmax <- pSigRational
  return ((xmin, ymin, zmin), (xmax, ymax, zmax))

pInput :: Parser [Hail]
pInput = some pHail <* eof

sol bounds str = let
  hails = parseOrError pInput str
  in length $ [(i,j) | 
    (h1,i) <- zip hails [1..],
    (h2,j) <- drop i $ zip hails [1..],
    p <- maybeToList $ findIntersection h1 h2,
    isPointWithinRange bounds p
    ]

test = sol (7, 27) <$> readSample 24
run = sol (200000000000000, 400000000000000) <$> readInput 24