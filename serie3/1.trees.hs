import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

--exampleTree1a = Node1a 1 (Leaf1a 35) (Leaf1a 36) 

data Tree1b = Leaf1b (Number, Number) | Node1b (Number, Number) Tree1b Tree1b

pp1b :: Tree1b -> RoseTree
pp1b (Node1b i t1 t2) = RoseNode (show i) [(pp1b t1), (pp1b t2)]
pp1b (Leaf1b i) = RoseNode (show i) []

data Tree1c = Leaf1c | Node1c Number Tree1c Tree1c

pp1c :: Tree1c -> RoseTree
pp1c (Node1c i t1 t2) = RoseNode (show i) [(pp1c t1), (pp1c t2)]
pp1c Leaf1c = RoseNode "" []

data Tree1d = Leaf1d (Number, Number) | Node1d [Tree1d]

pp1d :: Tree1d -> RoseTree
pp1d (Node1d trees) = RoseNode "" (map pp1d trees)
pp1d (Leaf1d i) = RoseNode (show i) []
