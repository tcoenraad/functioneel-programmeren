type Node = Int
type Graph = [(Node, [Node])]

ingraad :: Node -> Graph -> Int
ingraad n g = length $ filter (\x -> n `elem` (findNodes x g)) (allNodes g)

allNodes :: Graph -> [Node]
allNodes [] = []
allNodes ((n,_):g) = [n] ++ (allNodes g)

findNodes :: Node -> Graph -> [Node]
findNodes _ [] = []
findNodes n ((a, b):graph) | n == a = b
                           | otherwise = findNodes n graph
