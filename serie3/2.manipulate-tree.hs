import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

plus :: Number -> Tree1a -> Tree1a
plus x (Node1a i t1 t2) = Node1a (i+x) (plus x t1) (plus x t2)
plus x (Leaf1a i) = (Leaf1a (i+x))

kwadraat :: Tree1a -> Tree1a
kwadraat (Node1a i t1 t2) = Node1a (i^2) (kwadraat t1) (kwadraat t2)
kwadraat (Leaf1a i) = (Leaf1a (i^2))

mapTree :: (Number -> Number) -> Tree1a -> Tree1a
mapTree f (Node1a i t1 t2) = Node1a (f i) (mapTree f t1) (mapTree f t2)
mapTree f (Leaf1a i) = (Leaf1a (f i))

--showTree(pp1a(mapTree (^2) (Node1a 1 (Leaf1a 35) (Leaf1a 36))))

data Tree1b = Leaf1b (Number, Number) | Node1b (Number, Number) Tree1b Tree1b

pp1b :: Tree1b -> RoseTree
pp1b (Node1b i t1 t2) = RoseNode (show i) [(pp1b t1), (pp1b t2)]
pp1b (Leaf1b i) = RoseNode (show i) []

telopNode :: Tree1b -> Tree1a
telopNode (Node1b (i,x) t1 t2) = Node1a (i+x) (telopNode t1) (telopNode t2)
telopNode (Leaf1b (i,x)) = Leaf1a (i+x)

--showTree(pp1a(mapTree (^2) (Node1a 1 (Leaf1a 35) (Leaf1a 36))))

mapTree1b :: ((Number,Number) -> Number) -> Tree1b -> Tree1a
mapTree1b x (Node1b (i,j) t1 t2) = Node1a (x (i, j)) (mapTree1b x t1) (mapTree1b x t2)
mapTree1b x (Leaf1b (i,j)) = (Leaf1a (x (i, j)))

--showTree(pp1a(mapTree1b (\(x,y) -> x *y) (Node1b (1,2) (Leaf1b (3,5)) (Leaf1b (3,6)))))
--exampleTree2a = showTree(mapTree((+2) (Node1a 1 (Leaf1a 35) (Leaf1a 36))))
