import FPPrac
import RoseTree

data Tree1c = Leaf1c | Node1c Number Tree1c Tree1c

pp1c :: Tree1c -> RoseTree
pp1c (Node1c i t1 t2) = RoseNode (show i) [(pp1c t1), (pp1c t2)]
pp1c Leaf1c = RoseNode "" []

t_insert :: Number -> Tree1c -> Tree1c
t_insert a (Node1c i t1 t2) | i>=a = Node1c i (t_insert a t1) t2
                            | otherwise = Node1c i t1 (t_insert a t2)
t_insert a (Leaf1c) = Node1c a Leaf1c Leaf1c

--showTree(pp1c(t_insert 5 (Node1c 5 (Leaf1c ) (Leaf1c )))) 