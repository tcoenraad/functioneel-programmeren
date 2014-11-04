module Exercise where

type Node = Int
type Graph = [(Node, [Node])]

adjacentNodes :: Node -> Graph -> [Node]
adjacentNodes _ [] = []
adjacentNodes n ((a, b):graph) | n == a = b
                               | otherwise = adjacentNodes n graph

isCycle :: [Node] -> Graph -> Bool
isCycle nodes@(n:_) g | duplicates nodes = False -- node may only occur once
                      | otherwise = isCycle' (nodes ++ [n]) g

isCycle' :: [Node] -> Graph -> Bool
isCycle' [_] _ = True
isCycle' (n:m:ns) g | m `elem` (adjacentNodes n g) = isCycle' (m:ns) g
                    | otherwise = False

duplicates :: [Node] -> Bool
duplicates [] = False
duplicates (n:ns) | n `elem` ns = True
                  | otherwise = duplicates ns
