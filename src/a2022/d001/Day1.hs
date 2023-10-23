module Day1 where

import Lib
import Data.List as L
import Data.Function

parse :: String -> Int
parse s =
  let l = lines s
      calgroups = splitOnElem "" l
      numberOfNumber = map (sum . map readInt) calgroups
  in maximum numberOfNumber
  
parse2 :: String -> Int
parse2 s =
  let l = lines s
      calgroups = splitOnElem "" l
      numberOfNumber = map (sum . map readInt) calgroups
  in sum $ take 3 $ reverse $ sort numberOfNumber

execute :: String -> IO Int
execute path = do
  contents <- readFile path
  return (parse2 contents)