data Tree = Node Int [Tree] | Leaf Int deriving Show

vervangDoorMax :: Tree -> Tree
vervangDoorMax t = changeValues (findMax t) t

changeValues :: Int -> Tree -> Tree
changeValues x (Node v ts) = Node x (map (changeValues x) ts)
changeValues x (Leaf v) = Leaf x

findMax :: Tree -> Int
findMax t = maximum $ allValues t

allValues :: Tree -> [Int]
allValues (Node v ts) = v : concat (map allValues ts)
allValues (Leaf v) = [v]
