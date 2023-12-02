module D where
import Data.Char
import Lib;
import qualified Data.List as L
import qualified Data.Map as M

nums :: [(String, Int)]
nums = [
  ("one", 1),
  ("1", 1),
  ("two", 2),
  ("2", 2),
  ("three", 3),
  ("3", 3),
  ("four", 4),
  ("4", 4),
  ("five", 5),
  ("5", 5),
  ("six", 6),
  ("6", 6),
  ("seven", 7),
  ("7", 7),
  ("eight", 8),
  ("8", 8),
  ("nine", 9),
  ("9", 9)
  ]

firstDigit :: String -> Int
firstDigit = digitToInt . head . filter isDigit

toDigitSuffix :: String -> [Int]
toDigitSuffix s = concatMap (\(p, d) -> [d | L.isPrefixOf p s]) nums

firstDigit2 :: [String] -> Int
firstDigit2 s = head $ concatMap toDigitSuffix s

code :: String -> Int
code s = 10 * firstDigit2 (L.tails s) + firstDigit2 (reverse (L.tails s))

sol :: [String] -> Int
sol = sum . map code

test :: IO Int
test = runSample 1 sol
run :: IO Int
run = runInput 1 sol