data Tree = Node Int [Tree] | Leaf Int deriving Show
data PairTree = PairNode (Int, Int) [PairTree] | PairLeaf Int deriving Show

vervangDoorPaar :: Tree -> PairTree
vervangDoorPaar tree@(Node v ts) = PairNode (som tree, diepte tree) (map vervangDoorPaar ts)
vervangDoorPaar (Leaf v) = PairLeaf v

findMax :: Tree -> Int
findMax t = maximum $ allValues t

allValues :: Tree -> [Int]
allValues (Node v ts) = v : concat (map allValues ts)
allValues (Leaf v) = [v]

som :: Tree -> Int
som t = sum $ allValues t

diepte :: Tree -> Int
diepte (Node _ ts) = 1 + (maximum $ map diepte ts)
diepte (Leaf _) = 1
