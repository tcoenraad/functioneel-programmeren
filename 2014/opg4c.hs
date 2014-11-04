module Exercise where

import Data.List

data Graph = Graph{nodes :: [Node], edges :: [Edge]}
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

adjacentNodes :: Node -> Graph -> [Node]
adjacentNodes n Graph{edges=es} = adjacentNodes' n es

adjacentNodes' :: Node -> [Edge] -> [Node]
adjacentNodes' _ [] = []
adjacentNodes' n ((a, b):es) | a == n = b : adjacentNodes' n es
                        | b == n = a : adjacentNodes' n es
                        | otherwise = adjacentNodes' n es
