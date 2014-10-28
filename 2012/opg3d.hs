type Node = Int
type Graph = [(Node, [Node])]

bereikbaar :: Node -> Graph -> [Node]
bereikbaar n g = concat [y : (bereikbaar y g) | y <- findNodes n g]

findNodes :: Node -> Graph -> [Node]
findNodes _ [] = []
findNodes n ((a, b):graph) | n == a = b
                           | otherwise = findNodes n graph

duplicates :: [Node] -> Bool
duplicates [] = False
duplicates (n:ns) | n `elem` ns = True
                  | otherwise = duplicates ns

bevatCycle :: Graph -> Bool
bevatCycle g = any duplicates (map ((flip bereikbaar) g) (allNodes g))

allNodes :: Graph -> [Node]
allNodes [] = []
allNodes ((n,_):g) = [n] ++ (allNodes g)
