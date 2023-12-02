module D where

import qualified Data.Map as M
import Data.Tuple
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data Game = Game {gId :: Int, cubes :: [(Int, String)]} deriving (Show)

pGame :: Parser Game
pGame = do
  gId <- string "Game " *> Lex.decimal <* string ": "
  cubes <- sepBy ((,) <$> Lex.decimal <* string " " <*> choice [string "green", string "red", string "blue"]) (choice [string "; ", string ", "])
  return Game {..}

pProblem :: Parser [Game]
pProblem = sepEndBy pGame eol <* eof

isColorPossible :: (Int, String) -> Bool
isColorPossible (n, "red") = n <= 12
isColorPossible (n, "green") = n <= 13
isColorPossible (n, "blue") = n <= 14

isGamePossible :: Game -> Bool
isGamePossible Game {cubes} = all isColorPossible cubes

sol1 :: [Game] -> Int
sol1 = sum . map gId . filter isGamePossible

minSet :: Game -> Int
minSet Game {cubes} =
  let m :: M.Map String Int = M.fromListWith max $ map swap cubes
   in product $ M.elems m

sol2 :: [Game] -> Int
sol2 = sum . map minSet

test = sol2 . parseOrError pProblem <$> readInput 2
