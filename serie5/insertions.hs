import FPPrac
import FPPrac.Trees.RedBlackTree

data Colour = Red | Black | Grey deriving Eq
data RBTree = Tree Colour NodeType
data NodeType = Leaf | Node Number RBTree RBTree

rbZetom :: RBTree -> RBTreeG
rbZetom (Tree Red (Node i t1 t2)) = RBNodeG RedG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Black (Node i t1 t2)) = RBNodeG BlackG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Grey (Node i t1 t2)) = RBNodeG GreyG (show i)[rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Black Leaf) = RBNodeG RedG "" []
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
rootToBlack t@(Tree c (Node n t1 t2))
  | c == Red = (Tree Black (Node n t1 t2))
  | otherwise = t

-- showRBTree(rbZetom(rootToBlack(Tree Red (Node 2 (Tree Red (Node 1 (Tree Black Leaf) (Tree Black Leaf))) (Tree Red (Node 3 (Tree Black Leaf) (Tree Black Leaf)))))))

-- case a
colourFlip :: RBTree -> RBTree
colourFlip t@(Tree Black (Node n (Tree Red (Node n1 t4 t5)) (Tree Red (Node n2 t6 t7))))
  | any isRed [t4, t5, t6, t7] = Tree Red (Node n (Tree Black (Node n1 t4 t5)) (Tree Black (Node n2 t6 t7)))
  | otherwise = t

--Tree Black
--  (Node 2
--    (Tree Red 
--      (Node 1
--        (Tree Black Leaf)
--        (Tree Black Leaf)
--      )
--    )
--    (Tree Red
--      (Node 3
--        (Tree Black Leaf)
--        (Tree Black Leaf)
--      )
--    )
--  )

rebalance :: RBTree -> RBTree
-- case b
rebalance t1@(Tree c1 (Node a (Tree c2 (Node b (Tree c3 (Node c lll llr)) lr)) r))
  | c1 == Black && c2 == Red && c3 == Red = Tree Black (Node b (Tree Red (Node c lll llr)) (Tree Red (Node a lr r)))
  | otherwise = t1
rebalance t2@(Tree c1 (Node a l (Tree c2 (Node b rl (Tree c3 (Node c rrl rrr))))))
  | c1 == Black && c2 == Red && c3 == Red = Tree Black (Node b (Tree Red (Node a l rl)) (Tree Red (Node c rrl rrr)))
  | otherwise = t2
-- case c
rebalance t3@(Tree c1 (Node a (Tree c2 (Node b ll (Tree c3 (Node c lrl lrr)))) r))
  | c1 == Black && c2 == Red && c3 == Red = Tree Black (Node c (Tree Red (Node b ll lrl)) (Tree Red (Node a lrr r)))
  | otherwise = t3
rebalance t3@(Tree c1 (Node a r (Tree c2 (Node b (Tree c3 (Node c lrr lrl)) ll))))
  | c1 == Black && c2 == Red && c3 == Red = Tree Black (Node c (Tree Red (Node a r lrr)) (Tree Red (Node b lrl ll)))
  | otherwise = t3
  
-- showRBTree(rbZetom(Tree Black (Node 2 (Tree Red (Node 1 (Tree Red (Node 1 (Tree Black Leaf) (Tree Black Leaf))) (Tree Black Leaf))) (Tree Black Leaf)))) 
-- showRBTree(rbZetom(rebalance(Tree Black (Node 2 (Tree Red (Node 1 (Tree Red (Node 1 (Tree Black Leaf) (Tree Black Leaf))) (Tree Black Leaf))) (Tree Black Leaf))))) 
-- showRBTree(rbZetom(rebalance(Tree Black (Node 2 (Tree Red (Node 1 (Tree Black Leaf) (Tree Red (Node 1 (Tree Black Leaf) (Tree Black Leaf))))) (Tree Black Leaf))))) 
-- showRBTree(rbZetom(rebalance(Tree Black (Node 3 (Tree Black Leaf) (Tree Red (Node 8 (Tree Red (Node 6 (Tree Black Leaf) (Tree Black Leaf))) (Tree Black Leaf)))))))
-- showRBTree(rbZetom(rebalance(Tree Black (Node 4 (Tree Black Leaf) (Tree Red (Node 5 (Tree Black Leaf) (Tree Red (Node 6 (Tree Black Leaf) (Tree Black Leaf)))))))))

balancedInsert :: Number -> RBTree -> RBTree
balancedInsert a (Tree c (Node i t1 t2)) = rootToBlack(colourFlip(rebalance(insert a (Tree c (Node i t1 t2)))))

-- showRBTree(rbZetom(balancedInsert 4 (Tree Black (Node 7 (Tree Red (Node 3 (Tree Black Leaf) (Tree Black Leaf))) (Tree Black Leaf)))))
-- showRBTree(rbZetom(colourFlip(rebalance(insert 4 (Tree Black (Node 7 (Tree Red (Node 3 (Tree Black Leaf) (Tree Black Leaf))) (Tree Black Leaf)))))))