module D where

import Lib
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Tuple as T
import Debug.Trace

type Snafu = [Integer]

chrToInt :: M.Map Char Integer
chrToInt = M.fromList [('2', 2), ('1', 1), ('0', 0), ('-', -1), ('=', -2)]

intToChr :: M.Map Integer Char
intToChr = M.fromList $ map T.swap $ M.toList chrToInt

parseSnafu :: String -> Snafu
parseSnafu = map (\x -> M.findWithDefault 0 x chrToInt)

base :: Integer
base = 5
mBase = base `div` 2

snafuToDecimal :: Snafu -> Integer
snafuToDecimal = L.foldl' (\acc e -> (acc * 5) + e) 0

getDigits :: Integer -> Integer -> Integer -> Integer -> Snafu -> Snafu
getDigits _ _ _ 0 s = reverse s
getDigits t c r b s = let
  i = -(c - t + r) `div` b
  b' = b `div` base
  in getDigits t (c + i * b) (r - (base `div` 2) * b') b' (i:s)

digitsToSnafu :: Integer -> Snafu
digitsToSnafu d = let
  sumPower = L.scanl (\acc b -> (fst acc+b*mBase, b)) (0,0) $ map (base^) [0..]
  (r, b) = head $ L.dropWhile ((<d) . fst) sumPower
  in getDigits d 0 (r - mBase * b) b []

sol1 :: String -> String
sol1 = map (\x -> M.findWithDefault ' ' x intToChr) . digitsToSnafu . sum . map (snafuToDecimal . parseSnafu) . lines

test = sol1 <$> readSample 25 >>= putStrLn
run = sol1 <$> readInput 25 >>= putStrLn

