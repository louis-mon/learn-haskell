module D where

import Lib
import qualified Data.List as L
import Data.Either

import Text.Megaparsec hiding(State)
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

data Expr = Value Int | Old | Sum Expr Expr | Prod Expr Expr deriving (Show)
data Action = Action {mTest::Int, mTrue:: Int, mFalse::Int} deriving (Show)

data Monkey = Monkey {mId::Int,mItems::[Int], mOp :: Expr, mAction::Action} deriving (Show)

parseVar :: Parser Expr
parseVar = choice [Value <$> Lex.decimal, Old <$ string "old"]
parseExpr :: Parser Expr
parseExpr = makeExprParser parseVar [[InfixL (Prod <$ string " * ")],[InfixL (Sum <$ string " + ")]]

parseAction :: Parser Action
parseAction = Action <$>
  (string "  Test: divisible by " *> Lex.decimal <* eol) <*>
  (string "    If true: throw to monkey " *> Lex.decimal <* eol) <*>
  (string "    If false: throw to monkey " *> Lex.decimal)

parseMonkey :: Parser Monkey
parseMonkey = do
  mId <- string "Monkey " *> Lex.decimal <* char ':' <* eol
  mItems <- string "  Starting items: " *> sepBy Lex.decimal (string ", ") <* eol
  mOp <- string "  Operation: new = " *> parseExpr <* eol
  mAction <- parseAction
  return Monkey{..}

parseMonkeyList :: Parser [Monkey]
parseMonkeyList = endBy parseMonkey (many eol) <* eof

type Item = Modulo
type State = [(Int, [Item])]
type MonkeyIdx = Int
type ItemIdx = Int

newtype Modulo = Modulo {getModulo :: [(Int, Int)]} deriving (Show)

updateState :: State -> MonkeyIdx -> (MonkeyIdx, Item) -> State
updateState state monkeyIdx (newMonkeyIdx, newItem) =
  replaceAt newMonkeyIdx (\(oc, ol) -> (oc, ol++[newItem])) (replaceAt monkeyIdx (\(oc, ol) -> (oc+1, tail ol)) state)

doRound :: State -> [Monkey] -> State
doRound = L.foldl' doAction

applyMod :: (Int -> Int -> Int) -> Modulo -> Modulo -> Modulo
applyMod f (Modulo a) (Modulo b) = Modulo $ zipWith (\(m,x) (_,y) -> (m, f x y `mod` m)) a b

evalOp :: Expr -> Item -> Item
evalOp expr item = let
  inner e = case e of
    Sum a b -> applyMod (+) (inner a) (inner b)
    Prod a b -> applyMod (*) (inner a) (inner b)
    Old -> item
    Value a -> Modulo $ map (\(m, _) -> (m, a `mod` m)) $ getModulo item
  in inner expr

doAction :: State -> Monkey -> State
doAction state Monkey{mId, mAction=Action{mTrue, mFalse}, mOp} =
  L.foldl' (\acc item -> updateState acc mId (newMonkey item)) state (snd $ state !! mId)
  where
  newMonkey item = (if snd (getModulo newVal !! mId) == 0 then mTrue else mFalse, newVal)
    where
    newVal = evalOp mOp item

sol1 ls = answer
  where
    monkeys = fromRight [] $ parse parseMonkeyList "file" (unlines ls)
    initState :: State
    initState = map (\Monkey{mItems} -> (0, map (\val -> Modulo $ map (\Monkey{mAction} -> (mTest mAction, val `mod` mTest mAction)) monkeys) mItems)) monkeys
    states = L.iterate (`doRound` monkeys) initState
    answer = product $ take 2 $ reverse $ L.sort $ map fst $ states !! 10000

test = runSample "11" sol1

exec = runInput "11" sol1