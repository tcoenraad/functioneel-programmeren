import FPPrac
import FPPrac.Trees.RedBlackTree

data Color = Red | Black | Grey deriving Eq
data RBTree = Tree Color NodeType
data NodeType = Leaf | Node Number RBTree RBTree

rbZetom :: RBTree -> RBTreeG
rbZetom (Tree Red (Node i t1 t2)) = RBNodeG RedG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Black (Node i t1 t2)) = RBNodeG BlackG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Grey (Node i t1 t2)) = RBNodeG GreyG (show i)[rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Red Leaf) = RBNodeG RedG "" []
rbZetom (Tree Black Leaf) = RBNodeG BlackG "" []
rbZetom (Tree Grey Leaf) = RBNodeG GreyG "" []

isRed :: RBTree -> Bool
isRed (Tree Red _) = True
isRed _ = False

insert :: Number -> RBTree -> RBTree
insert n (Tree c (Node i t1 t2)) | n <= i = (Tree c (Node i (insert n (t1)) t2)) 
						               | otherwise = (Tree c (Node i t1 (insert n (t2)))) 
insert n (Tree c Leaf) = Tree Red (Node n (Tree c Leaf) (Tree c Leaf))

-- showRBTree(rbZetom(insert 1 (Tree Black(Node 2 (Tree Red (Node 1 (Tree Black Leaf) (Tree Black Leaf))) (Tree Red (Node 3 (Tree Black Leaf) (Tree Black Leaf)))))))

rootToBlack :: RBTree -> RBTree
rootToBlack t@(Tree Red (Node n t1 t2))
  | isRed t1 || isRed t2 = (Tree Black (Node n t1 t2))
  | otherwise = t
rootToBlack t = t

-- showRBTree(rbZetom(rootToBlack(Tree Red (Node 2 (Tree Red (Node 1 (Tree Black Leaf) (Tree Black Leaf))) (Tree Red (Node 3 (Tree Black Leaf) (Tree Black Leaf)))))))
