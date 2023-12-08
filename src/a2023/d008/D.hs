module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.List.Split as Split
import Data.Maybe
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as Lex

data Input = Input {dirs:: [Char], paths :: M.Map String (String, String)}

pDirs :: Parser [Char]
pDirs = lexeme $ some (char 'L' <|> char 'R')

pNode :: Parser String
pNode = lexeme $ some alphaNumChar

pPath :: Parser (String, (String, String))
pPath = do
  origin <- lexeme pNode
  _ <- symbol "="
  _ <- symbol "("
  first <- lexeme pNode
  _ <- symbol ","
  second <- lexeme pNode
  _ <- symbol ")"
  return (origin, (first, second))

pInput :: Parser Input
pInput = do
  dirs <- pDirs
  paths <- some pPath
  return $ Input dirs $ M.fromList paths
  
followPath :: Input -> String -> Char -> String
followPath Input{paths} current c = case c of
  'L' -> fst $ paths M.! current
  'R' -> snd $ paths M.! current

sol1 :: Input -> Int
sol1 input@Input{dirs} =
  fromJust $ L.elemIndex "ZZZ" $ L.scanl (followPath input) "AAA" $ concat $ repeat dirs
  
sol2 :: Input -> Int
sol2 input@Input{dirs,paths} = let
  starts = filter (L.isSuffixOf "A") (M.keys paths)
  isEnd = L.isSuffixOf "Z"
  cmpOne start = fromJust $ L.findIndex isEnd $ L.scanl (followPath input) start $ concat $ repeat dirs
  idx = map cmpOne starts
  in foldr1 lcm idx

test = sol2 . parseOrError pInput <$> readSample 8
run = sol2 . parseOrError pInput <$> readInput 8