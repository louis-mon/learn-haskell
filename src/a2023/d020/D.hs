module D where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

data ModuleType = Broad | FlipFlop | Conj deriving (Show, Eq)

data Module = Module {mType :: ModuleType, name :: String, emits :: [String]} deriving (Show)

type Input = M.Map String Module

pModule :: Parser Module
pModule = do
  mType <- fromMaybe Broad <$> optional (try $ choice [FlipFlop <$ char '%', Conj <$ char '&'])
  name <- some letterChar
  _ <- string " -> "
  emits <- sepBy (some letterChar) (string ", ")
  return Module {..}

pInput :: Parser Input
pInput = M.fromList . map (\x -> (name x, x)) <$> sepEndBy pModule eol <* eof

data Signal = Low | High deriving (Eq, Ord, Show)

type ConjState = M.Map String Signal

data ModuleState = ModuleState {flipState :: M.Map String Bool, conjState :: M.Map String ConjState} deriving (Eq, Show)

data St = St {input :: Input, zeroModules :: ModuleState, modules :: ModuleState, cnt :: M.Map Signal Integer, iter :: Integer}

countSignals :: [(String, String, Signal)] -> State St Integer
countSignals sigs = do
  St {input, zeroModules, modules = ModuleState {flipState = ofl, conjState = oc}, cnt, iter} <- get
  if any (\(_, t, p) -> t == "rx" && p == Low) sigs
    then return iter
    else
      if null sigs
        then do
          {--if iter >= endIter then
            return $ product $ M.elems cnt--}
          modify (\st -> st {iter = iter + 1})
          countSignals [("button", "broadcaster", Low)]
        else do
          nextSigs <-
            concat
              <$> forM
                sigs
                ( \(fromModule, toModule, sigType) -> do
                    case M.lookup toModule input of
                      Just sigModule -> do
                        case mType sigModule of
                          Broad -> return $ map (toModule,,sigType) (emits sigModule)
                          FlipFlop -> case sigType of
                            High -> return []
                            Low -> do
                              st@St {modules = ms@ModuleState {flipState = fl}} <- get
                              let prevState = fl M.! toModule
                              put st {modules = ms {flipState = M.adjust not toModule fl}}
                              return $ map (toModule,,if prevState then Low else High) (emits sigModule)
                          Conj -> do
                            nst@St {modules = nms@ModuleState {conjState = conjSt, flipState = fl}} <- get
                            let newConj = M.adjust (M.insert fromModule sigType) toModule conjSt
                            put nst {modules = nms {conjState = newConj}}
                            let allHigh = all (== High) $ M.elems $ newConj M.! toModule
                                toEmit = if allHigh then Low else High
                            return $ map (toModule,,toEmit) (emits sigModule)
                      Nothing -> return []
                )
          ModuleState {flipState = nf, conjState = nc} <- gets (\St {modules} -> modules)
          let flipDiffs = filter (uncurry (/=)) $ zip (M.assocs ofl) (M.assocs nf)
          modify (\st -> st {cnt = M.unionWith (+) cnt (M.fromListWith (+) $ map ((,1) . (\(_, _, s) -> s)) sigs)})
          trace (show flipDiffs) $ countSignals nextSigs

sol :: String -> Integer
sol str =
  let input = parseOrError pInput str
      flipState = M.fromList $ map (,False) $ M.keys input
      parentsOf pName = map fst $ filter (\(_, m) -> pName `elem` emits m) $ M.assocs input
      conjState = M.fromList $ map (\(name, _) -> (name, M.fromList $ map (,Low) $ parentsOf name)) $ filter (\(_, m) -> mType m == Conj) $ M.assocs input
      st = ModuleState {flipState, conjState}
   in evalState (countSignals []) St {input, cnt = M.empty, modules = st, zeroModules = st, iter = 0}

endIter = 1000

test = sol <$> readSample 20

run = sol <$> readInput 20
