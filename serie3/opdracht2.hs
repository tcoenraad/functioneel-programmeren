import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

plusX :: Number -> Tree1a -> Tree1a
plusX x (Node1a i t1 t2) = Node1a (i+x) (plusX x t1) (plusX x t2)
plusX x (Leaf1a i) = (Leaf1a (i+x))

--exampleTree2a = showTree(pp1a(plusX 4 (Node1a 1 (Leaf1a 35) (Leaf1a 36))))