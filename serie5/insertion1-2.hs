import FPPrac
import FPPrac.Trees.RedBlackTree

data Col = R | B | G deriving Eq -- Red, Black, Grey 
data RBTree = T Col NodeType
data NodeType = L | N Number RBTree RBTree

rbZetom :: RBTree -> RBTreeG
rbZetom (T R (N i t1 t2)) = RBNodeG RedG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (T B (N i t1 t2)) = RBNodeG BlackG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (T G (N i t1 t2)) = RBNodeG GreyG (show i)[rbZetom(t1), rbZetom(t2)]
rbZetom (T R L) = RBNodeG RedG "" []
rbZetom (T B L) = RBNodeG BlackG "" []
rbZetom (T G L) = RBNodeG GreyG "" []

insert :: Number -> RBTree -> RBTree
insert n (T c (N i t1 t2)) | n <= i = (T c (N i (insert n (t1)) t2)) 
						   | otherwise = (T c (N i t1 (insert n (t2)))) 
insert n (T c L) = T R (N n (T c L) (T c L))

-- showRBTree(rbZetom(insert 1 (T B (N 2 (T R (N 1 (T B L) (T B L))) (T R (N 3 (T B L) (T B L)))))))

rootToBlack :: RBTree -> RBTree
rootToBlack (T c (N i t1 t2)) = T B (N i t1 t2)

-- showRBTree(rbZetom(rootToBlack(T R (N 2 (T R (N 1 (T B L) (T B L))) (T R (N 3 (T B L) (T B L)))))))