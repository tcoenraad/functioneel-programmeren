import FPPrac
import FPPrac.Trees.RedBlackTree

data Col = R | B | G -- Red, Black, Grey
data RBTree = T Col NodeType
data NodeType = L | N Number RBTree RBTree

rbZetom :: RBTree -> RBTreeG
rbZetom (T R (N i t1 t2)) = RBNodeG RedG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (T B (N i t1 t2)) = RBNodeG BlackG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (T G (N i t1 t2)) = RBNodeG GreyG (show i)[rbZetom(t1), rbZetom(t2)]
rbZetom (T R L) = RBNodeG RedG "" []
rbZetom (T B L) = RBNodeG BlackG "" []
rbZetom (T G L) = RBNodeG GreyG "" []

-- showRBTree(rbZetom(T B (N 1 (T R (N 2 (T B L) (T B L))) (T R (N 3 (T B L) (T B L))))))