module Exercise where

import Data.List

data Tree = Node Int [Tree] | Leaf Int deriving Show

allValues :: Tree -> [Int]
allValues (Node i ts) = i : concat (map allValues ts)
allValues (Leaf i) = [i]

median :: Tree -> Int
median t = s !! (((length s) `quot` 2) - 1) where
  s = sort (allValues t)

minstensMed :: Tree -> Tree
minstensMed t = minstensMed' t (median t)

minstensMed' :: Tree -> Int -> Tree
minstensMed' (Node i ts) m | i < m = Node 0 (map ((flip minstensMed') m) ts)
                           | otherwise = Node i (map ((flip minstensMed') m) ts)
minstensMed' (Leaf i) m | i < m = Leaf 0
                        | otherwise = Leaf i
