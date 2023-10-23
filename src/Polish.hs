module Polish(polish) where

import Data.Char
import qualified Data.Map as M
import qualified Data.List as L

getOp :: M.Map String (Float -> Float -> Float)
getOp = M.fromList [("*", (*)), ("+", (+)), ("/", (/)), ("-", (-))]

combine :: [Float] -> String -> [Float]
combine st x
 | all isDigit x = (read x::Float):st
 | otherwise = op a b:sts
    where
      Just op = M.lookup x getOp
      (a:b:sts) = st
computeSt :: [String] -> Float
computeSt = head . L.foldl' combine []

polish :: String -> Float
polish = computeSt . words