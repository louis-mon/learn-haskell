module D where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import qualified Geo as Geo
import qualified Grid2d as G
import Lib
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

-- map pipes to direction of inputs
pipes :: M.Map Char (Geo.Point, Geo.Point)
pipes =
  M.fromList
    [ ('|', (Geo.up, Geo.down)),
      ('-', (Geo.left, Geo.right)),
      ('F', (Geo.up, Geo.left)),
      ('L', (Geo.down, Geo.left)),
      ('7', (Geo.up, Geo.right)),
      ('J', (Geo.down, Geo.right))
    ]

nextPos :: G.Grid2d Char -> (Geo.Point, Geo.Point) -> Maybe (Geo.Point, Geo.Point)
nextPos m (p, d) = case G.lookup2d (Geo.toTuple (Geo.plus p d)) m of
  Nothing -> Nothing
  Just c -> do
    (d1, d2) <- M.lookup c pipes
    let res
          | d == d1 = Just $ Geo.neg d2
          | d == d2 = Just $ Geo.neg d1
          | otherwise = Nothing
    nd <- res
    return (Geo.plus p d, nd)

computePath :: String -> (G.Grid2d Char, [(Geo.Point, Geo.Point)])
computePath input =
  let m = G.parseFromLines $ lines input
      startPos = Geo.fromTuple $ fromJust $ G.findIndex (== 'S') m
      startPipe = head $ mapMaybe (\d -> nextPos m (startPos, d)) Geo.way4
      path = L.unfoldr (\p -> (p,) <$> nextPos m p) startPipe
      (lst, lstDir) = last path
      lPos = Geo.plus lst lstDir
   in (m, [(startPos, Geo.sub (fst $ head path) startPos)] ++ path ++ [(lPos, Geo.sub startPos lPos)])

typeOfS :: [(Geo.Point, Geo.Point)] -> Char
typeOfS l =
  let (_, Geo.Point x1 y1) = head l
      (_, Geo.Point x2 y2) = last l
   in case x1 of
        0 -> case x2 of
          0 -> '|'
          1 -> case y1 of
            -1 -> 'J'
            1 -> '7'
          -1 -> case y1 of
            -1 -> 'L'
            1 -> 'F'
        1 -> case x2 of
          1 -> '-'
          0 -> case y2 of
            1 -> 'L'
            -1 -> 'F'
        -1 -> case x2 of
          -1 -> '-'
          0 -> case y2 of
            1 -> 'J'
            -1 -> '7'

sol2 :: String -> Int
sol2 str =
  let (m, path) = computePath str
      byX :: M.Map Int (S.Set Int)
      byX = M.fromListWith S.union $ map (\(p, _) -> (Geo.x p, S.singleton $ Geo.y p)) path
      getP p = case fromJust $ G.lookup2d p m of
        'S' -> typeOfS path
        c -> c
      inConcave :: Geo.Point -> Bool
      inConcave Geo.Point {x, y} = case M.lookup x byX of
        Nothing -> False
        Just line ->
          let rayCast :: Int -> (Char, Int) -> (Char, Int)
              rayCast cy (dir, cnt) = case getP (x, cy) of
                'S' -> (dir, cnt)
                '-' -> (dir, cnt + 1)
                '|' -> (dir, cnt)
                'L' -> ('L', cnt)
                'J' -> ('J', cnt)
                'F' -> (dir, if dir == 'L' then cnt else cnt + 1)
                '7' -> (dir, if dir == 'J' then cnt else cnt + 1)
              onLine = S.member y line
              nb = snd $ foldr rayCast ('.', 0) (fst $ S.split y line)
           in not onLine && odd nb
   in length $ filter inConcave $ map Geo.fromTuple $ G.keys m

sol1 :: String -> Int
sol1 = (`div` 2) . length . snd . computePath

test = sol2 <$> readSample 10

run = sol2 <$> readInput 10
