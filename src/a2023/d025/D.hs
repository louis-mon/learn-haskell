module D where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Graph = M.Map String (S.Set String)

pLine :: Parser (String, S.Set String)
pLine = do
  from <- many letterChar
  _ <- string ": "
  to <- sepBy (many letterChar) (char ' ')
  return (from, S.fromList to)

pGraph :: Parser Graph
pGraph = do
  ls <- sepEndBy pLine eol <* eof
  let g1 = M.fromList ls
  let g2 = M.fromListWith S.union $ concatMap (\(f, t) -> map (,S.singleton f) $ S.toList t) ls
  return $ M.unionWith S.union g1 g2

data TraverseEdge = TraverseEdge {traversedLinks :: M.Map (S.Set String) Int, traversedV :: S.Set String}

traverseEdges :: Graph -> S.Set (String, String) -> State TraverseEdge ()
traverseEdges _ s | null s = return ()
traverseEdges g toVisit = do
  connected <-
    concat
      <$> forM
        (S.toList toVisit)
        ( \(vfrom, vto) -> do
            visited <- gets traversedV
            if S.member vto visited
              then return []
              else do
                let insertIn Nothing = Just 1
                    insertIn (Just x) = Just (x + 1)
                modify
                  ( \s@TraverseEdge {traversedLinks, traversedV} ->
                      s
                        { traversedLinks = M.alter insertIn (S.fromList [vfrom, vto]) traversedLinks,
                          traversedV = S.insert vto traversedV
                        }
                  )
                return $ map (vto,) $ S.toList $ M.findWithDefault S.empty vto g
        )
  traverseEdges g (S.fromList connected)

countFromV :: Graph -> State TraverseEdge (M.Map Int (S.Set (S.Set String)))
countFromV g = do
  forM_
    (M.keys g)
    ( \v -> do
        modify (\s -> s {traversedV = S.empty})
        traverseEdges g (S.singleton (v, v))
    )
  m <- gets traversedLinks
  return $ M.fromListWith S.union $ map (\(a, b) -> (b, S.singleton a)) $ M.toList m

countTrav :: Graph -> [S.Set String] -> Int
countTrav g cut =
  let gFlat = S.fromList $ map (\(a, b) -> S.fromList [a, b]) $ concatMap (\(k, v) -> map (k,) (S.toList v)) $ M.toList g
      gFlatCut = S.toList $ S.difference gFlat (S.fromList cut)
      g' = M.fromListWith S.union $ concatMap ((\[a, b] -> [(a, S.singleton b), (b, S.singleton a)]) . S.toList) gFlatCut
      pt = head (M.keys g)
      st = execState (traverseEdges g' (S.singleton (pt, pt))) TraverseEdge {traversedLinks = M.empty, traversedV = S.empty}
      subLen = length $ traversedV st
  in subLen * (length g - subLen)

solPrep str =
  let g = parseOrError pGraph str
      iniSt = TraverseEdge {traversedLinks = M.empty, traversedV = S.empty}
   in evalState (countFromV g) iniSt

sol str =
  let g = parseOrError pGraph str
   --in countTrav g [S.fromList ["rtt", "zcj"], S.fromList ["hxq", "txl"], S.fromList ["gxv", "tpn"]]
   in countTrav g [S.fromList ["rtt", "zcj"], S.fromList ["hxq", "txl"], S.fromList ["gxv", "tpn"]]

highestProb :: M.Map Int (S.Set (S.Set String))
highestProb = M.fromList [(1211, S.fromList [S.fromList ["clb", "fcm"], S.fromList ["fkh", "jvs"]]), (1212, S.fromList [S.fromList ["czz", "npb"], S.fromList ["fjv", "gkg"], S.fromList ["ggf", "ggs"]]), (1215, S.fromList [S.fromList ["fkh", "nhb"]]), (1219, S.fromList [S.fromList ["bxx", "jfd"]]), (1222, S.fromList [S.fromList ["dcx", "dqd"]]), (1225, S.fromList [S.fromList ["bbj", "bkq"]]), (1233, S.fromList [S.fromList ["csg", "gtc"]]), (1235, S.fromList [S.fromList ["bpd", "djs"]]), (1236, S.fromList [S.fromList ["bbs", "hrh"], S.fromList ["blx", "lqh"]]), (1238, S.fromList [S.fromList ["cxs", "gcm"]]), (1239, S.fromList [S.fromList ["bhn", "clx"], S.fromList ["bjt", "cbj"]]), (1242, S.fromList [S.fromList ["bdt", "lzp"]]), (1248, S.fromList [S.fromList ["cmg", "xkg"], S.fromList ["drt", "gjx"]]), (1249, S.fromList [S.fromList ["gtd", "gxc"]]), (1251, S.fromList [S.fromList ["ckm", "qlq"]]), (1252, S.fromList [S.fromList ["bgr", "cxm"], S.fromList ["bqs", "chc"]]), (1254, S.fromList [S.fromList ["gmz", "htd"]]), (1258, S.fromList [S.fromList ["ffg", "glt"]]), (1259, S.fromList [S.fromList ["cbf", "lzh"], S.fromList ["dtg", "hrx"]]), (1296, S.fromList [S.fromList ["fjv", "gfd"]]), (1452, S.fromList [S.fromList ["rtt", "zcj"]]), (1453, S.fromList [S.fromList ["hxq", "txl"]]), (1455, S.fromList [S.fromList ["gxv", "tpn"]])]

test = solPrep <$> readSample 25

run = sol <$> readInput 25