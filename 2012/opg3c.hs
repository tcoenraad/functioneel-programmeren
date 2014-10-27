type Node = Int
type Graph = [(Node, [Node])]

findNodes :: Node -> Graph -> [Node]
findNodes _ [] = []
findNodes n ((a, b):graph) | n == a = b
                           | otherwise = findNodes n graph

isCycle :: [Node] -> Graph -> Bool
isCycle nodes@(n:ns) g | duplicates nodes = False
                       | otherwise = isCycle' (nodes ++ [n]) g

isCycle' :: [Node] -> Graph -> Bool
isCycle' [_] _ = True
isCycle' (n:m:ns) g | m `elem` (findNodes n g) = isCycle' (m:ns) g
                    | otherwise = False

duplicates :: [Node] -> Bool
duplicates [] = False
duplicates (n:ns) | n `elem` ns = True
                  | otherwise = duplicates ns
