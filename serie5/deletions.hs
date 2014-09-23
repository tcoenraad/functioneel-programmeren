import FPPrac
import FPPrac.Trees.RedBlackTree

data Colour = Red | Black | Grey deriving Eq
data RBTree = Tree Colour NodeType
data NodeType = Leaf | Node Number RBTree RBTree 

rbZetom :: RBTree -> RBTreeG
rbZetom (Tree Red (Node i t1 t2)) = RBNodeG RedG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Black (Node i t1 t2)) = RBNodeG BlackG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Grey (Node i t1 t2)) = RBNodeG GreyG (show i)[rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Red Leaf) = RBNodeG RedG "" []
rbZetom (Tree Black Leaf) = RBNodeG BlackG "" []
rbZetom (Tree Grey Leaf) = RBNodeG GreyG "" []

leftmostValue :: RBTree -> RBTree
leftmostValue t@(Tree c (Node n (Tree _ Leaf) t2)) = t
leftmostValue (Tree c (Node n t1 t2)) = leftmostValue (t1)

-- leftmostValue(Tree Black (Node 3 (Tree Red (Node 1 (Tree Red Leaf) (Tree Red (Node 2 (Tree Red Leaf) (Tree Red Leaf))))) (Tree Red Leaf))) 

removeLeftmostNode :: RBTree -> RBTree
removeLeftmostNode (Tree _ (Node _ (Tree _ Leaf) (Tree _ Leaf))) = Tree Grey Leaf
removeLeftmostNode (Tree _ (Node _ (Tree _ Leaf) t2)) = t2
removeLeftmostNode (Tree c (Node n t1 t2)) = (Tree c (Node n (removeLeftmostNode t1) t2))

greyColourFlip :: RBTree -> RBTree
-- all black
greyColourFlip (Tree Black (Node p (Tree Grey g) (Tree Black (Node s (Tree Black l) (Tree Black r))))) = (Tree Grey (Node p (Tree Black g) (Tree Red (Node s (Tree Black l) (Tree Black r)))))
greyColourFlip (Tree Black (Node p (Tree Black (Node s (Tree Black r) (Tree Black l))) (Tree Grey g))) = (Tree Grey (Node p (Tree Red (Node s (Tree Black r) (Tree Black l))) (Tree Black g)))
-- l is red
greyColourFlip (Tree c1 (Node p (Tree Grey g) (Tree Black (Node s (Tree Red (Node l (Tree Black a) (Tree Black b))) (Tree c2 r))))) = (Tree c1 (Node l (Tree Black (Node p (Tree Black g) (Tree Black a))) (Tree Black (Node s (Tree Black b) (Tree c2 r)))))
greyColourFlip (Tree c1 (Node p (Tree Black (Node s (Tree c2 r) (Tree Red (Node l (Tree Black b) (Tree Black a))))) (Tree Grey g))) = (Tree c1 (Node l (Tree Black (Node s (Tree c2 r) (Tree Black b))) (Tree Black (Node p (Tree Black a) (Tree Black g)))))
