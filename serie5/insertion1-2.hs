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

colourFlip :: RBTree -> RBTree
colourFlip t@(Tree Black (Node n (Tree Red (Node n1 t4 t5)) (Tree Red (Node n2 t6 t7))))
  | any isRed [t4, t5, t6, t7] = Tree Red (Node n (Tree Black (Node n1 t4 t5)) (Tree Black (Node n2 t6 t7)))
  | otherwise = t

--Tree Black
--  (Node 2
--    (Tree Red 
--      (Node 1
--        (Tree Red Leaf)
--        (Tree Red Leaf)
--      )
--    )
--    (Tree Red
--      (Node 3
--        (Tree Red Leaf)
--        (Tree Red Leaf)
--      )
--    )
--  )

rebalance1 :: RBTree -> RBTree
rebalance1 t@(Tree c1 (Node n (Tree c2 (Node n1 (Tree c3 (Node n2 t4 t5)) t6)) t7))
  | c1==Black && c2==Red && c3 == Red = Tree Black (Node n1 (Tree Red (Node n2 t4 t5)) (Tree Red (Node n t6 t7)))
  | otherwise = t

-- showRBTree(rbZetom(Tree Black (Node 2 (Tree Red (Node 1 (Tree Red (Node 1 (Tree Red Leaf) (Tree Red Leaf))) (Tree Red Leaf))) (Tree Red Leaf)))) 
-- showRBTree(rbZetom(rebalance1(Tree Black (Node 2 (Tree Red (Node 1 (Tree Red (Node 1 (Tree Red Leaf) (Tree Red Leaf))) (Tree Red Leaf))) (Tree Red Leaf))))) 