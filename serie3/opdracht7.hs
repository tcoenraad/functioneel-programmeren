import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

--showTree(pp1a(Node1a 1 (Node1a 5 (Leaf1a 8) (Leaf1a 3)) (Leaf1a 3)))

vervang :: String -> Number -> Tree1a -> Tree1a
vervang p n (Node1a i t1 t2) | p == "" = Node1a n t1 t2
                             | (head p) == 'l' && (length p) >0 = Node1a i (vervang (tail p) n t1) t2
                             | otherwise = Node1a i t1 (vervang (tail p) n t2)                            
vervang p n (Leaf1a i) = Leaf1a i

--showTree(pp1a(vervang "l" 2 (Node1a 1 (Node1a 5 (Leaf1a 8) (Leaf1a 3)) (Leaf1a 3))))
