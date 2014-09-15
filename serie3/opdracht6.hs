import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

totDiepte :: Number -> Tree1a -> Tree1a
totDiepte m (Node1a i t1 t2) | i>m = totDiepte m (Leaf1a i)
                             | otherwise = Node1a i  (totDiepte m t1) (totDiepte m t2)
totDiepte m (Leaf1a i) = Leaf1a i

--showTree(pp1a(totDiepte 4 (Node1a 1 (Node1a 5 (Leaf1a 8) (Leaf1a 3)) (Leaf1a 3))))