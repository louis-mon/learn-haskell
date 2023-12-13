module D where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data Line = Line {row :: String, parts :: [Int]}

nCopy :: Int
nCopy = 5

pLine :: Parser Line
pLine = do
  row <- some (choice $ map char ['.', '#', '?'])
  _ <- char ' '
  parts <- sepBy Lex.decimal (char ',')
  return Line {row = L.intercalate "?" $ L.replicate nCopy row, parts = concat $ L.replicate nCopy parts}

pInput :: Parser [Line]
pInput = sepEndBy pLine eol <* eof

type St = M.Map (Int, Int) Integer

cntRec :: (String, [Int]) -> State St Integer
cntRec (s, []) = if elem '#' s then return 0 else return 1
cntRec (s, next@(p : xp))
  | length s < p = return 0
  | otherwise = do
    m <- get
    case M.lookup key m of
      Just v -> return v
      Nothing -> do
        cd <- caseDot
        cp <- casePart
        nm <- get
        put $ M.insert key (cp + cd) nm
        return (cp + cd)
  where
    (c : xs) = s
    caseDot :: State St Integer
    caseDot = if elem c ".?" then cntRec (xs, next) else return 0
    casePart :: State St Integer
    casePart = if all (`elem` "#?") fs && nextNoPart then cntRec (drop (p + 1) s, xp) else return 0
    nextNoPart = null rs || elem (head rs) ".?"
    (fs, rs) = L.splitAt p s
    key = (length s, length next)

countLine :: Line -> Integer
countLine Line {row, parts} =
  evalState (cntRec (row, parts)) M.empty

sol :: String -> Integer
sol = sum . map countLine . parseOrError pInput

test = sol <$> readSample 12

run = sol <$> readInput 12
