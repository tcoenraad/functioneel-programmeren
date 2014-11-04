module Exercise where

import Data.List

data Graph = Graph{nodes :: [Node], edges :: [Edge]} deriving Show
type Node = Int
type Edge = (Int, Int)

connected :: Graph -> Bool
connected Graph{nodes=[_]} = True
connected g@Graph{nodes=ns} = and $ map (\x -> x `elem` reachables) ns where
  reachables = reachable (head ns) g

reachable :: Node -> Graph -> [Node]
reachable n g = reachable' n g []

reachable' :: Node -> Graph -> [Node] -> [Node]
reachable' n g vns = concat [y : (reachable' y g (y:vns)) | y <- (adjacentNodes n g) \\ vns]

adjacentEdges :: Node -> Graph -> [Edge]
adjacentEdges n Graph{edges=es} = adjacentEdges' n es

adjacentEdges' :: Node -> [Edge] -> [Edge]
adjacentEdges' _ [] = []
adjacentEdges' n (e@(a,b):es) | a == n || b == n = e : adjacentEdges' n es
                      | otherwise = adjacentEdges' n es

adjacentNodes :: Node -> Graph -> [Node]
adjacentNodes n Graph{edges=es} = adjacentNodes' n es

adjacentNodes' :: Node -> [Edge] -> [Node]
adjacentNodes' _ [] = []
adjacentNodes' n ((a, b):es) | a == n = b : adjacentNodes' n es
                        | b == n = a : adjacentNodes' n es
                        | otherwise = adjacentNodes' n es

removeNode :: Node -> Graph -> Graph
removeNode n g@Graph{nodes=ns, edges=es} = Graph{nodes=ns', edges=es'} where
  ns' = ns \\ [n]
  es' = es \\ adjacentEdges n g

bevatKnoop :: Graph -> Bool
bevatKnoop g@Graph{nodes=ns} = or $ map (\n -> c < countSubgraphs (removeNode n g)) ns where
  c = countSubgraphs g

countSubgraphs :: Graph -> Int
countSubgraphs Graph{nodes=[]} = 0
countSubgraphs g@Graph{nodes=(n:ns), edges=es} = 1 + countSubgraphs Graph{nodes=ns', edges=es} where
  ns' = ns \\ reachable n g
