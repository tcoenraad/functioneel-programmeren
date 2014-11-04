module Exercise where

data Tree = Node Int [Tree] | Leaf Int deriving Show

diepte :: Tree -> Int
diepte (Node _ ts) = 1 + (maximum $ map diepte ts)
diepte (Leaf _) = 1
