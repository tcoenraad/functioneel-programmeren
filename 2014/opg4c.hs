import Data.List

data Graph = Graph{nodes :: [Node], edges :: [Edge]}
type Node = Int
type Edge = (Int, Int)

connected :: Graph -> Bool
connected g@Graph{nodes=[n]} = True
connected g@Graph{nodes=ns} = and $ map (\x -> x `elem` reachables) ns where
  reachables = reachable (head ns) g

reachable :: Node -> Graph -> [Node]
reachable n g = reachable' n g []

reachable' :: Node -> Graph -> [Node] -> [Node]
reachable' n g vns = concat [y : (reachable' y g (y:vns)) | y <- (adjNodes n g) \\ vns]

adjNodes :: Node -> Graph -> [Node]
adjNodes n Graph{edges=es} = adjNodes' n es

adjNodes' :: Node -> [Edge] -> [Node]
adjNodes' n [] = []
adjNodes' n ((a, b):es) | a == n = b : adjNodes' n es
                         | b == n = a : adjNodes' n es
                         | otherwise = adjNodes' n es
