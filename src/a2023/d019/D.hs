module D where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data Cond = Cond {field :: Char, op :: Ordering, value :: Integer} deriving (Show)

data Rule = Rule {when :: Maybe Cond, link :: String} deriving (Show)

data Work = Work {name :: String, rules :: [Rule]} deriving (Show)

type Object = M.Map Char Integer

data Input = Input {works :: M.Map String Work, objs :: [Object]} deriving (Show)

pCond :: Parser Cond
pCond = do
  field <- letterChar
  op <- choice [LT <$ char '<', GT <$ char '>']
  value <- Lex.decimal
  _ <- char ':'
  return Cond {..}

pRule :: Parser Rule
pRule = do
  when <- optional (try pCond)
  link <- some letterChar
  return Rule {..}

pWork :: Parser Work
pWork = do
  name <- some letterChar
  _ <- char '{'
  rules <- sepBy pRule (char ',')
  _ <- char '}'
  return Work {..}

pField :: Parser (Char, Integer)
pField = (,) <$> letterChar <* char '=' <*> Lex.decimal

pObject :: Parser Object
pObject = do
  _ <- char '{'
  values <- sepBy pField (char ',')
  _ <- char '}'
  return $ M.fromList values

pInput :: Parser Input
pInput = do
  works <- sepEndBy pWork eol
  _ <- eol
  objs <- sepEndBy pObject eol
  _ <- eof
  return Input {works = M.fromList $ map (\w -> (name w, w)) works, objs}

findNewFlow :: [Rule] -> Object -> String
findNewFlow [Rule {link}] _ = link
findNewFlow (Rule {when = Just Cond {field, op, value}, link} : xs) obj =
  if compare (obj M.! field) value == op
    then link
    else findNewFlow xs obj

isObjectAccepted :: Input -> String -> Object -> Bool
isObjectAccepted input@Input {works} flow obj =
  let newFlow = findNewFlow (rules $ works M.! flow) obj
   in case newFlow of
        "A" -> True
        "R" -> False
        _ -> isObjectAccepted input newFlow obj

sol :: String -> Integer
sol str =
  let input = parseOrError pInput str
      accepted = filter (isObjectAccepted input "in") (objs input)
   in sum $ map (sum . M.elems) accepted

type ObjRange = M.Map Char (Integer, Integer)

splitRange :: Integer -> (Integer, Integer) -> ((Integer, Integer), (Integer, Integer))
splitRange v (a, b) = ((a, v), (v, b))

zeroRange :: (Integer, Integer) -> Bool
zeroRange (a, b) = b <= a

countFromRules :: Input -> [Rule] -> ObjRange -> Integer
countFromRules input [Rule {link}] obj = countFromLink input link obj
countFromRules input (Rule {when = Just Cond {field, op, value}, link} : xs) obj =
  let follow v = if zeroRange v then 0 else countFromLink input link (M.insert field v obj)
      next v = if zeroRange v then 0 else countFromRules input xs (M.insert field v obj)
   in case op of
        LT ->
          let (v1, v2) = splitRange value (obj M.! field)
           in follow v1 + next v2
        GT ->
          let (v1, v2) = splitRange (value + 1) (obj M.! field)
           in follow v2 + next v1

countFromLink :: Input -> String -> ObjRange -> Integer
countFromLink input@Input {works} flow obj = case flow of
  "A" -> product . map (\(a, b) -> b - a) $ M.elems obj
  "R" -> 0
  _ -> countFromRules input (rules $ works M.! flow) obj

sol2 :: String -> Integer
sol2 str =
  let input = parseOrError pInput str
      iniRange = M.fromList $ map (,(1, 4001)) "xmas"
   in countFromLink input "in" iniRange

test = sol2 <$> readSample 19

run = sol2 <$> readInput 19
