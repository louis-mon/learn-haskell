module Graph(Graph(..), shortestPath) where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Debug.Trace

class Graph g a where
  neighs :: g -> a -> [a]
  
type ShortestPathResult a = M.Map a Int

shortestPath :: forall a g . (Graph g a, Ord a, Show a) => [a] -> g -> ShortestPathResult a
shortestPath points g = let
  rec :: ShortestPathResult a -> [a] -> ShortestPathResult a
  rec r [] = r
  rec r ls = let
    withNeighs = concatMap (\x -> map (x,) $ neighs g x) ls
    notVisited = filter ((\(_,b) -> M.notMember b r) :: (a, a) -> Bool) withNeighs
    newState = foldr (\(a,b) acc -> M.insertWith min b (fromJust (M.lookup a acc) + 1) acc) r notVisited
    next = S.toList $ S.fromList $ map snd notVisited
    in rec newState next
  in rec (M.fromList $ map (,0) points) points