module Exercise where

import Data.List

type Node = Int
type Graph = [(Node, [Node])]

bereikbaar :: Node -> Graph -> [Node]
bereikbaar n g = bereikbaar' n g []

bereikbaar' :: Node -> Graph -> [Node] -> [Node]
bereikbaar' n g vns = concat [y : (bereikbaar' y g (y:vns)) | y <- (adjacentNodes n g) \\ vns]

adjacentNodes :: Node -> Graph -> [Node]
adjacentNodes _ [] = []
adjacentNodes n ((a, b):graph) | n == a = b
                               | otherwise = adjacentNodes n graph

duplicates :: [Node] -> Bool
duplicates [] = False
duplicates (n:ns) | n `elem` ns = True
                  | otherwise = duplicates ns

bevatCycle :: Graph -> Bool
bevatCycle g = or (map (\n -> n `elem` (bereikbaar n g)) (allNodes g))

allNodes :: Graph -> [Node]
allNodes [] = []
allNodes ((n,_):g) = [n] ++ (allNodes g)
