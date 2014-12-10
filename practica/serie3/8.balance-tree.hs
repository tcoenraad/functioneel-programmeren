import FPPrac
import RoseTree

data Tree1c = Leaf1c | Node1c Number Tree1c Tree1c

pp1c :: Tree1c -> RoseTree
pp1c (Node1c i t1 t2) = RoseNode (show i) [(pp1c t1), (pp1c t2)]
pp1c Leaf1c = RoseNode "" []

isBalanced :: Tree1c -> Bool
isBalanced (Node1c _ t1 t2) = (isBalanced t1) && (isBalanced t2) && abs(depth t1 - depth t2) <= 1
isBalanced Leaf1c = True

depth :: Tree1c -> Int
depth (Node1c _ t1 t2) = 1 + maximum [(depth t1), (depth t2)]
depth Leaf1c = 1

-- unbalanced: Node1c 10 (Leaf1c) (Node1c 5 (Node1c 8 Leaf1c Leaf1c) Leaf1c)
-- balanced: Node1c 10 (Node1c 3 Leaf1c Leaf1c) (Node1c 5 (Node1c 8 Leaf1c Leaf1c) Leaf1c)

makeList :: Tree1c -> [Number]
makeList (Node1c i t1 t2) =  (makeList t1) ++ [i] ++ (makeList t2)
makeList (Leaf1c) = []

treeToBalancedTree :: Tree1c -> Tree1c
treeToBalancedTree tree = treeToBalancedTree'(makeList tree)

treeToBalancedTree' :: [Number] -> Tree1c
treeToBalancedTree' (x:xs) = Node1c x (treeToBalancedTree' (take (1 + ((length xs) `quot` 2)) xs)) (treeToBalancedTree' (drop (1 + ((length xs) `quot` 2)) xs))
treeToBalancedTree' [] = Leaf1c
