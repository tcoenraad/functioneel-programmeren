module Exercise where

type Node = Int
type Graph = [(Node, [Node])]

ingraad :: Node -> Graph -> Int
ingraad n g = length $ filter (\x -> n `elem` (adjacentNodes x g)) (allNodes g)

allNodes :: Graph -> [Node]
allNodes [] = []
allNodes ((n,_):g) = [n] ++ (allNodes g)

adjacentNodes :: Node -> Graph -> [Node]
adjacentNodes _ [] = []
adjacentNodes n ((a, b):graph) | n == a = b
                               | otherwise = adjacentNodes n graph
