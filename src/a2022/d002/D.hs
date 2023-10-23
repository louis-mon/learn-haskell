import Lib

import Data.Char

toSym :: Char -> Int
toSym c = ord c - ord 'A'

compareScore :: Char -> Char -> Int
compareScore a b =
  let an = ord a - ord 'A'
      bn = ord b - ord 'X'
      win = bn == an + 1 || (bn == 0 && an == 2)
      draw = an == bn
  in if win then 6 else (if draw then 3 else 0)
{-
compareScore a b = case (a,b) of
 ('A', 'Y') -> 6
 ('B', 'Z') -> 6
 ('C', 'X') -> 6
 ('A', 'X') -> 3
 ('B', 'Y') -> 3
 ('C', 'Z') -> 3
 _ -> 0
 -}
      
indivScore :: Char -> Int
indivScore a = case a of
  'X' -> 1
  'Y' -> 2
  'Z' -> 3
 

scoreLine :: String -> Int
scoreLine (a:_:b:_)  = indivScore b + compareScore a b

score :: [String] -> Int
score = sum . map scoreLine

execute :: String -> IO Int
execute path = do
  score <$> readFileLines path