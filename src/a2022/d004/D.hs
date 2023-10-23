module D where
  
import Lib
import Data.List as L

isSubSetPair2 :: String -> Bool
isSubSetPair2 line =
 let [[ra1, ra2],[rb1, rb2]] = map getRange $ splitOnElem ',' line
     getRange = map readInt . splitOnElem '-'
 in (ra1 >= rb1 && ra1 <= rb2) || (ra2 >= rb1 && ra2 <= rb2)
   || (rb1 >= ra1 && rb2 <= ra2) || (rb2 >= ra1 && rb2 <= ra2)

isSubSetPair :: String -> Bool
isSubSetPair line =
 let [[ra1, ra2],[rb1, rb2]] = map getRange $ splitOnElem ',' line
     getRange = map readInt . splitOnElem '-'
 in (ra1 >= rb1 && ra2 <= rb2) || (rb1 >= ra1 && rb2 <= ra2)

solve = length . filter isSubSetPair2
execute p = solve <$> readFileLines p