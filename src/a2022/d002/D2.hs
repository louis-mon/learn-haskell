module D2 where

import Lib

import Data.Char

toSym :: Char -> Int
toSym c = ord c - ord 'A'

compareScore :: Char -> Char -> Int
compareScore a b =
 1 + ((aVal + delta) `mod` 3) + sc
 where (delta, sc) = case b of
                 'X' -> (-1, 0)
                 'Y' -> (0, 3)
                 'Z' -> (1, 6)
       aVal = ord a - ord 'A' + 3 

scoreLine :: String -> Int
scoreLine (a:_:b:_)  = compareScore a b

score :: [String] -> Int
score = sum . map scoreLine

execute :: String -> IO Int
execute path = do
  score <$> readFileLines path
