module D where

import Control.Monad.State
import qualified Data.List as L
import Data.List.Split as Split
import qualified Data.Map as M
import qualified Data.Set as S
import Geo
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data Instr = Instr {dir :: Point, value :: Int, color :: String}

pLine :: Parser Instr
pLine = do
  dir <- letterToDir <$> letterChar <* char ' '
  value <- Lex.decimal <* string " (#"
  color <- some alphaNumChar <* string ")"
  return Instr {dir, value, color}

pInput :: Parser [Instr]
pInput = sepEndBy pLine eol <* eof

type Node = (Point, Point, String)

letterToDir :: Char -> Point
letterToDir 'U' = up
letterToDir 'D' = down
letterToDir 'L' = left
letterToDir 'R' = right

genPos :: [Instr] -> [Node]
genPos =
  snd
    . L.mapAccumL (\p (dir, color) -> (plus p dir, (p, dir, color))) (Point 0 0)
    . concatMap (\Instr {dir, value, color} -> replicate value (dir, color))

data St = St {visited :: S.Set Point}

fillGraphRec :: S.Set Point -> State St Int
fillGraphRec toVisit = do
  st@St {visited} <- get
  let newPoints = S.difference toVisit visited
  if null newPoints
    then return $ length visited
    else do
      put $ st {visited = S.union visited newPoints}
      fillGraphRec $ S.unions $ map (\p -> S.fromList $ map (plus p) way4) $ S.toList newPoints

sol :: String -> Int
sol str =
  let pos = genPos . parseOrError pInput $ str
      border = S.fromList $ map (\(p, _, _) -> p) pos
      firstVisit = S.fromList $ map (\(p, d, _) -> plus p (turnRight d)) pos
   in evalState (fillGraphRec firstVisit) St {visited = border}

shoelaceFormula :: [Point] -> Integer
shoelaceFormula pts =
  let tuples = Split.divvy 2 1 $ pts ++ [head pts]
      area = sum $ map (\[Point x1 y1, Point x2 y2] -> toInteger $ x1 * y2 - x2 * y1) tuples
   in abs area `div` 2

genPos2 :: [(Point, Int)] -> [Point]
genPos2 =
  snd
    . L.mapAccumL (\p (dir, value) -> (plus p $ scalePoint value dir, p)) (Point 0 0)

instrToDir :: Instr -> (Point, Int)
instrToDir Instr {color} =
  let (v, [d]) = splitAt 5 color
      iTD = case d of
        '0' -> right
        '1' -> down
        '2' -> left
        '3' -> up
   in (iTD, parseOrError Lex.hexadecimal v)

sol2 :: String -> Integer
sol2 str =
  let dirs = map instrToDir $ parseOrError pInput str
      pos = genPos2 dirs
      borderSize = toInteger $ sum $ map snd dirs
   in shoelaceFormula pos + (borderSize `div` 2) + 1

test = sol2 <$> readSample 18

run = sol2 <$> readInput 18
