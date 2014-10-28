data Tree = Node Int [Tree] | Leaf Int deriving Show

som :: Tree -> Int
som t = sum $ allValues t

allValues :: Tree -> [Int]
allValues (Node v ts) = v : concat (map allValues ts)
allValues (Leaf v) = [v]
