module Exercise where

data Tree = Node Int [Tree] | Leaf Int deriving Show

padnaar :: Int -> Tree -> [Int]
padnaar i t | (length ps) > 0 = head ps
            | otherwise = error "No path can be found"
              where ps = filter (elem i) (allPaths t)

allPaths :: Tree -> [[Int]]
allPaths (Node i ts) = [i:paths | paths <- concat $ map allPaths ts]
allPaths (Leaf i) = [[i]]
