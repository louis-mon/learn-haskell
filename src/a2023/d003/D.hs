module D where

import Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Grid2d as G
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

pOther :: Parser [Char]
pOther = many (noneOf ['0' .. '9'])

pLine :: Parser [(Int, Int, Int)]
pLine = do
  _ <- optional pOther
  sepEndBy
    do
      p1 <- getSourcePos
      d <- Lex.decimal
      p2 <- getSourcePos
      return (d, sourceColI p1, sourceColI p2)
    pOther

type NumP = (Int, Int, Int, Int)

data Input = Input {getGrid :: G.Grid2d Char, getNums :: [NumP]} deriving (Show)

getNum :: NumP -> Int
getNum (n, _, _, _) = n

isSym :: Char -> Bool
isSym c = not (isDigit c) && c /= '.'

processInput :: Input -> [Int]
processInput (Input grid nums) =
  [ n | (n, x1, x2, y) <- nums, let cont = G.lookup2ds (G.pContour (x1, y) (x2, y + 1)) grid, any isSym cont
  ]

type SymRef = (Char, (Int, Int))

getNghOfPart :: Int -> Int -> Int -> Input -> [SymRef]
getNghOfPart x1 x2 y (Input grid _) =
  [ (c, ngh) | ngh <- G.pContour (x1, y) (x2, y + 1), c <- Maybe.maybeToList $ G.lookup2d ngh grid, isSym c
  ]

processInput2 :: Input -> Int
processInput2 input@(Input _ nums) =
  let allParts =
        [(n, getNghOfPart x1 x2 y input) | (n, x1, x2, y) <- nums]
      bySym = M.fromListWith (++) [(s, [n]) | (n, syms) <- allParts, s <- syms]
      byGear = filter (\((s, _), ns) -> s == '*' && length ns == 2) $ M.assocs bySym
   in sum $ map (\(_, ns) -> product ns) byGear

parseNum :: String -> [(Int, Int, Int)]
parseNum = parseOrError pLine

parseLine :: (Int, String) -> [NumP]
parseLine (y, s) = map (\(n, x1, x2) -> (n, x1, x2, y)) $ parseNum s

parseInput :: [String] -> Input
parseInput ls =
  let nums = concatMap parseLine $ zip [0 ..] ls
      grid = G.parseFromLines ls
   in Input grid nums

--sol1 = sum . processInput . parseInput
sol = processInput2 . parseInput

test = runSample 3 sol

run = runInput 3 sol
