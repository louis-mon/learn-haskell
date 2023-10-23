module PrefixTree(Tree(..), make, insertPath) where

import Data.List as L
import Lib
  
data Tree a b = Tree a b [Tree a b]

instance (Show a, Show b) => Show (Tree a b) where
  show t = let
    rec :: (Show a, Show b) => String -> Tree a b -> String
    rec pref (Tree a b children) = pref ++ show a ++ ": " ++ show b ++ "\n" ++
      (L.intercalate "\n" $ map (rec (pref ++ "  ")) children)
    in rec "" t

getK :: Tree a b -> a
getK (Tree a _ _) = a

make :: a -> b -> Tree a b
make x y = Tree x y []

insertPath :: (Eq a) => [a] -> a -> b -> Tree a b -> Tree a b
insertPath [] a b (Tree x y children) = Tree x y (Tree a b []:children)
insertPath xs x y (Tree c d children) = case findIndex ((== head xs) . getK) children of
  Just childIndex -> Tree c d (replaceAt childIndex (insertPath (tail xs) x y) children)