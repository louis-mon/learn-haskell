module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Char

updateHash :: Int -> Char -> Int
updateHash acc v = let
  code = acc + Data.Char.ord v
  in (code * 17) `mod` 256

hashCode :: String -> Int
hashCode  = L.foldl' updateHash 0

sol :: String -> Int
sol str = let
  sequences = splitOnElem ',' $ filter (/='\n') str
  in sum $ map hashCode sequences

type Box = [(String, Int)]

type St = M.Map Int Box

data Action = Set Int | Remove deriving Show

pSet :: Parser Action
pSet = Set <$> (char '=' *> Lex.decimal)

pRemove :: Parser Action
pRemove = Remove <$ char '-'

pMsg :: Parser (String, Action)
pMsg = do
  label <- some alphaNumChar
  action <- choice [pRemove, pSet]
  eof
  return (label, action)

insertInBox :: (String, Int) -> Box -> Box
insertInBox e@(label, _) l = let
  (bef, after) = L.span ((/=label) . fst) l
  in bef ++ [e] ++ drop 1 after

handleMessage :: St -> String -> St
handleMessage st msg = let
  (label, action) = parseOrError pMsg msg
  boxIdx = hashCode label
  update = case action of
    Remove -> filter ((/= label) . fst)
    Set focal -> insertInBox (label, focal)
  in M.adjust update boxIdx st

boxFocal :: (Int, Box) -> Int
boxFocal (i, b) = (i+1)*sum (zipWith (\j (_,f) -> j*f) [1..] b)

sol2 :: String -> Int
sol2 str = let
  sequences = splitOnElem ',' $ filter (/='\n') str
  stInit = M.fromList $ zip [0..] (replicate 256 [])
  stEnd = L.foldl' handleMessage stInit sequences
  in sum $ map boxFocal $ M.assocs stEnd
  
test = sol2 <$> readSample 15
run = sol2 <$> readInput 15

