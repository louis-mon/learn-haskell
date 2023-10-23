module D where

import Lib
import Data.List as L
import Data.Maybe

len :: Int
len = 14

getSeqs :: String -> [String]
getSeqs s = drop len $ transpose [L.replicate (i - 1) ' ' ++ s | i <- [1..len]]

solve :: String -> Int
solve = (+(len+1)) . fromJust . L.findIndex ((len ==) .  length . nub) . getSeqs

