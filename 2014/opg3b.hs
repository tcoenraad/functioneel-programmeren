module Exercise where

data Tree = Node Int Tree Tree | Leaf Int

zoekMax :: Tree -> Int
zoekMax t = maximum $ allValues t

allValues :: Tree -> [Int]
allValues (Node i t1 t2) = [i] ++ allValues t1 ++ allValues t2
allValues (Leaf i) = [i]

-- alternative
zoekMax' :: Tree -> Int
zoekMax' (Node i t1 t2) = max i (max (zoekMax' t1) (zoekMax' t2))
zoekMax' (Leaf i) = i
