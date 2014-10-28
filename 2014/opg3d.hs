data Tree = Node Int Tree Tree | Leaf Int

sorted :: Tree -> Bool
sorted (Node i t1 t2) | i < zoekMin t2 && i > zoekMax t1 = sorted t1 && sorted t2
                      | otherwise = False
sorted (Leaf i) = True

allValues :: Tree -> [Int]
allValues (Node i t1 t2) = [i] ++ allValues t1 ++ allValues t2
allValues (Leaf i) = [i]

zoekMin :: Tree -> Int
zoekMin t = minimum $ allValues t

zoekMax :: Tree -> Int
zoekMax t = maximum $ allValues t
