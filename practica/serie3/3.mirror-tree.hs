import FPPrac
import RoseTree

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Node1a i t1 t2) = RoseNode (show i) [(pp1a t1), (pp1a t2)]
pp1a (Leaf1a i) = RoseNode (show i) []

binspiegel :: Tree1a -> Tree1a
binspiegel (Node1a i t1 t2) = Node1a i (binspiegel t2) (binspiegel t1)
binspiegel (Leaf1a i) = Leaf1a i

--showTree(pp1a (binspiegel(Node1a 1 (Leaf1a 3) (Leaf1a 2))))

data Tree1d = Leaf1d (Number, Number) | Node1d [Tree1d]

pp1d :: Tree1d -> RoseTree
pp1d (Node1d trees) = RoseNode "" (map pp1d trees)
pp1d (Leaf1d i) = RoseNode (show i) []

--showTree(pp1d (Node1d [Leaf1d (1,1), Leaf1d (2,1), Leaf1d (4,1), Leaf1d (5,1)]))

binspiegelD :: Tree1d -> Tree1d
binspiegelD (Node1d trees) = Node1d (map binspiegelD (reverse trees))
binspiegelD (Leaf1d (i,j)) = Leaf1d (j,i)

--showTree(pp1d (binspiegelD(Node1d [Leaf1d (1,1), Leaf1d (2,1), Leaf1d (4,1), Leaf1d (5,1)])))