module D where

import Lib
import Data.List as L
import Data.List.Split
import Data.Char

prioChar :: Char -> Int
prioChar c = if isUpper c then 27 + ord c - ord 'A' else 1 + ord c - ord 'a'

prioPack :: String -> Int
prioPack str = prioChar $ head inter
 where
   (a, b) = splitAt (length str `div` 2) str
   inter = a `intersect` b

sol :: [String] -> Int
sol = sum . map prioPack

prioPack2 :: [String] -> Int
prioPack2 = prioChar . head . foldr1 L.intersect

sol2 :: [String] -> Int
sol2 = sum . map prioPack2 . chunksOf 3

execute :: String -> IO Int
execute p = sol2 <$> readFileLines p
