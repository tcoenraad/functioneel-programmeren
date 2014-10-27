type Node = Int
type Graph = [(Node, [Node])]

bereikbaar :: Node -> Graph -> [Node]
bereikbaar n g = concat [y : (bereikbaar y g) | y <- findNodes n g]

findNodes :: Node -> Graph -> [Node]
findNodes _ [] = []
findNodes n ((a, b):graph) | n == a = b
                           | otherwise = findNodes n graph
