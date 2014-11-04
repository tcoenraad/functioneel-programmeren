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
minstensMed t = minstensMed' (median t) t

minstensMed' :: Int -> Tree -> Tree
minstensMed' m (Node i ts) | i < m = Node 0 $ map (minstensMed' m) ts
                           | otherwise = Node i $ map (minstensMed' m) ts
minstensMed' m (Leaf i) | i < m = Leaf 0
                        | otherwise = Leaf i
