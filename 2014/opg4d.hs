import Data.List

data Graph = Graph{nodes :: [Node], edges :: [Edge]} deriving Show
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

adjEdges :: Node -> Graph -> [Edge]
adjEdges n Graph{edges=es} = adjEdges' n es

adjEdges' :: Node -> [Edge] -> [Edge]
adjEdges' _ [] = []
adjEdges' n (e@(a,b):es) | a == n || b == n = e : adjEdges' n es
                      | otherwise = adjEdges' n es

adjNodes :: Node -> Graph -> [Node]
adjNodes n Graph{edges=es} = adjNodes' n es

adjNodes' :: Node -> [Edge] -> [Node]
adjNodes' _ [] = []
adjNodes' n ((a, b):es) | a == n = b : adjNodes' n es
                        | b == n = a : adjNodes' n es
                        | otherwise = adjNodes' n es

removeNode :: Node -> Graph -> Graph
removeNode n g@Graph{nodes=ns, edges=es} = Graph{nodes=ns', edges=es'} where
  ns' = ns \\ [n]
  es' = es \\ adjEdges n g

bevatKnoop :: Graph -> Bool
bevatKnoop g@Graph{nodes=ns} = or $ map (\n -> c < countSubgraphs (removeNode n g)) ns where
  c = countSubgraphs g

countSubgraphs :: Graph -> Int
countSubgraphs g@Graph{nodes=[]} = 0
countSubgraphs g@Graph{nodes=(n:ns), edges=es} = 1 + countSubgraphs Graph{nodes=ns', edges=es} where
  ns' = ns \\ reachable n g
