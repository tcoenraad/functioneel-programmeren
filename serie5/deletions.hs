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
