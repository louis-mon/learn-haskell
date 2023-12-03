module D where

import Lib
import Data.Char
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type NumP = (Int, Int, Int)
data Line = Line {nums::[(Int, Int, Int)], chars::M.Map Int Char} deriving Show
type Input = M.Map Int Line

getNum :: NumP -> Int
getNum (n, s, e) = n

isSym :: Char -> Bool
isSym c = not (isDigit c) && c /= '.'

isPart :: NumP -> Int -> Input -> Bool
isPart (n, s, e) lineI input = let
  Line{chars = lchars} = input M.! lineI
  left = (s > 0) && isSym (lchars M.! (s-1))
  right = (e <= M.size lchars - 1) && isSym (lchars M.! e)
  subLine = [x | x <- [s-1..e], x >= 0 && x < M.size lchars]
  top = (lineI > 0) && any (\x -> isSym (chars (input M.! (lineI - 1)) M.! x) ) subLine
  bottom = (lineI < M.size input - 1) && any (\x -> isSym (chars (input M.! (lineI + 1)) M.! x) ) subLine
  in left || right || top || bottom

type SymRef = (Char, (Int, Int))

findParts :: NumP -> Int -> Input -> [(Char, (Int, Int))]
findParts (n, s, e) lineI input = let
  Line{chars = lchars} = input M.! lineI
  left = [(lchars M.! (s-1), (s-1, lineI)) | (s > 0) && isSym (lchars M.! (s-1))]
  right = [(lchars M.! e, (e, lineI)) | (e <= M.size lchars - 1) && isSym (lchars M.! e)]
  subLine = [x | x <- [s-1..e], x >= 0 && x < M.size lchars]
  top = [(c, (x, lineI - 1)) | lineI > 0, x <- subLine, let c = chars (input M.! (lineI - 1)) M.! x, isSym c ]
  bottom = [(c, (x, lineI + 1)) | lineI < M.size input - 1, x <- subLine, let c = chars (input M.! (lineI + 1)) M.! x, isSym c ]
  in left ++ right ++ top ++ bottom

cntLine :: Line -> Int -> Input -> [Int]
cntLine Line{nums} lineI input = map getNum $ filter (\n -> isPart n lineI input) nums

groupGearsLine :: Line -> Int -> Input -> [(Int, [SymRef])]
groupGearsLine Line{nums} lineI input = map (\n -> (getNum n, findParts n lineI input)) nums

processInput2 :: Input -> Int
processInput2 input = let
  allParts = concatMap (\(i, x) -> groupGearsLine x i input) $ M.assocs input
  bySym = M.fromListWith (++) [(s, [n]) | (n, syms) <- allParts, s <- syms]
  byGear = filter (\((s, _), ns) -> s == '*' && length ns == 2) $ M.assocs bySym
  in sum $ map (\(_, ns) -> product ns) byGear

processInput :: Input -> [Int]
processInput input = concatMap (\(i, x) -> cntLine x i input) $ M.assocs input

parseNum :: String -> Int -> [NumP] -> [NumP]
parseNum [] _ l = l
parseNum s si l = let
  (st, e) = L.span isDigit s
  res = [(readInt st, si, si+length st) | not (null st)]
  in parseNum (if null e then [] else tail e) (1 +si+length st) res++l


parseLine :: String -> Line
parseLine s = let
  nums = parseNum s 0 []
  in Line{nums, chars=M.fromList (zip [0..] s)}

parseInput :: [String] -> Input
parseInput ls = M.fromList $ zip [0..] (map parseLine ls)

sol1 = sum . processInput . parseInput
sol2 = processInput2 . parseInput

test = runSample 3 sol2
run = runInput 3 sol2