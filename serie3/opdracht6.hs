import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

totDiepte :: Number -> Tree1a -> Tree1a
totDiepte m (Node1a i t1 t2) | m/=0 = Node1a i (totDiepte (m-1) t1) (totDiepte (m-1) t2)
                             | otherwise = Leaf1a i
totDiepte m (Leaf1a i) = Leaf1a i

--showTree(pp1a(totDiepte 2 (Node1a 1 (Node1a 5 (Leaf1a 8) (Leaf1a 3)) (Leaf1a 3))))
