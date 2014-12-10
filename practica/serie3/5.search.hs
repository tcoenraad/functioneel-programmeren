import FPPrac
import RoseTree

data Tree1c = Leaf1c | Node1c Number Tree1c Tree1c

pp1c :: Tree1c -> RoseTree
pp1c (Node1c i t1 t2) = RoseNode (show i) [(pp1c t1), (pp1c t2)]
pp1c Leaf1c = RoseNode "" []

zoek :: Number -> Tree1c -> Tree1c
zoek n Leaf1c = error "Could not find n in given tree"
zoek n (Node1c i t1 t2) | i > n = zoek n t1
                        | i < n = zoek n t2
                        | otherwise = Node1c i t1 t2
