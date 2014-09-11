import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

plusX :: Number -> Tree1a -> RoseTree
plusX x(Node1a i t1 t2) = pp1a (Node1a (i+x) t1 t2)
plusX x(Leaf1a i) = pp1a (Leaf1a(i+x))