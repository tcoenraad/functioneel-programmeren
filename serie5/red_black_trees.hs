import FPPrac
import FPPrac.Trees.RedBlackTree

data Colour = Red | Black | Grey deriving Eq
data RBTree = Tree Colour NodeType
data NodeType = Leaf | Node Number RBTree RBTree

rbZetom :: RBTree -> RBTreeG
rbZetom (Tree Red (Node i t1 t2)) = RBNodeG RedG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Black (Node i t1 t2)) = RBNodeG BlackG (show i) [rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Grey (Node i t1 t2)) = RBNodeG GreyG (show i)[rbZetom(t1), rbZetom(t2)]
rbZetom (Tree Black Leaf) = RBNodeG BlackG "" []
rbZetom (Tree Grey Leaf) = RBNodeG GreyG "" []

isRed :: RBTree -> Bool
isRed (Tree Red _) = True
isRed _ = False

insert :: Number -> RBTree -> RBTree
insert n (Tree c (Node i t1 t2)) | n <= i = (Tree c (Node i (insert n (t1)) t2)) 
                                 | otherwise = (Tree c (Node i t1 (insert n (t2)))) 
insert n (Tree c Leaf) = Tree Red (Node n (Tree c Leaf) (Tree c Leaf))

rootToBlack :: RBTree -> RBTree
rootToBlack t@(Tree c (Node n t1 t2))
  | c == Red = (Tree Black (Node n t1 t2))
  | otherwise = t

-- case a
colourFlip :: RBTree -> RBTree
colourFlip t@(Tree Black (Node n (Tree Red (Node n1 t4 t5)) (Tree Red (Node n2 t6 t7))))
  | any isRed [t4, t5, t6, t7] = Tree Red (Node n (Tree Black (Node n1 t4 t5)) (Tree Black (Node n2 t6 t7)))
  | otherwise = t
colourFlip t = t

rebalance :: RBTree -> RBTree
-- case b
rebalance t1@(Tree c1 (Node a (Tree c2 (Node b (Tree c3 (Node c lll llr)) lr)) r))
  | c1 == Black && c2 == Red && c3 == Red = Tree Black (Node b (Tree Red (Node c lll llr)) (Tree Red (Node a lr r)))
  | otherwise = t1
-- mirror b
rebalance t2@(Tree c1 (Node a l (Tree c2 (Node b rl (Tree c3 (Node c rrl rrr))))))
  | c1 == Black && c2 == Red && c3 == Red = Tree Black (Node b (Tree Red (Node a l rl)) (Tree Red (Node c rrl rrr)))
  | otherwise = t2
-- case c
rebalance t3@(Tree c1 (Node a (Tree c2 (Node b ll (Tree c3 (Node c lrl lrr)))) r))
  | c1 == Black && c2 == Red && c3 == Red = Tree Black (Node c (Tree Red (Node b ll lrl)) (Tree Red (Node a lrr r)))
  | otherwise = t3
-- mirror c
rebalance t3@(Tree c1 (Node a r (Tree c2 (Node b (Tree c3 (Node c lrr lrl)) ll))))
  | c1 == Black && c2 == Red && c3 == Red = Tree Black (Node c (Tree Red (Node a r lrr)) (Tree Red (Node b lrl ll)))
  | otherwise = t3

rebalanceTree :: RBTree -> RBTree
rebalanceTree t@(Tree c Leaf) = t
rebalanceTree t = (Tree c (Node n (rebalanceTree t1) (rebalanceTree t2))) where
  (Tree c (Node n t1 t2)) = colourFlip t

balancedInsert :: Number -> RBTree -> RBTree
balancedInsert a (Tree c (Node i t1 t2)) = rootToBlack(rebalanceTree(insert a (Tree c (Node i t1 t2))))

leftmostValue :: RBTree -> Number
leftmostValue (Tree c (Node n (Tree _ Leaf) _)) = n
leftmostValue (Tree c (Node n t1 _)) = leftmostValue t1

removeLeftmostNode :: RBTree -> RBTree
-- if red node, return just right subtree
removeLeftmostNode (Tree Red (Node _ (Tree _ Leaf) t2)) = t2
-- if black node and subtree Red, return just right subtree
removeLeftmostNode (Tree Black (Node _ (Tree _ Leaf) t2@(Tree Red _))) = t2
-- if both leaves, return grey leaf
removeLeftmostNode (Tree _ (Node _ (Tree _ Leaf) (Tree _ Leaf))) = Tree Grey Leaf
-- if left leaf, right node, return right substree
removeLeftmostNode (Tree _ (Node _ (Tree _ Leaf) t2)) = t2
-- else, look further on the left
removeLeftmostNode (Tree c (Node n t1 t2)) = (Tree c (Node n (removeLeftmostNode t1) t2))

greyColourFlip :: RBTree -> RBTree
-- all black
greyColourFlip (Tree Black (Node p (Tree Grey g) (Tree Black (Node s (Tree Black l) (Tree Black r))))) = (Tree Grey (Node p (Tree Black g) (Tree Red (Node s (Tree Black l) (Tree Black r)))))
greyColourFlip (Tree Black (Node p (Tree Black (Node s (Tree Black r) (Tree Black l))) (Tree Grey g))) = (Tree Grey (Node p (Tree Red (Node s (Tree Black r) (Tree Black l))) (Tree Black g)))
-- l is red
greyColourFlip (Tree c1 (Node p (Tree Grey g) (Tree Black (Node s (Tree Red (Node l (Tree Black a) (Tree Black b))) (Tree c2 r))))) = (Tree c1 (Node l (Tree Black (Node p (Tree Black g) (Tree Black a))) (Tree Black (Node s (Tree Black b) (Tree c2 r)))))
greyColourFlip (Tree c1 (Node p (Tree Black (Node s (Tree c2 r) (Tree Red (Node l (Tree Black b) (Tree Black a))))) (Tree Grey g))) = (Tree c1 (Node l (Tree Black (Node s (Tree c2 r) (Tree Black b))) (Tree Black (Node p (Tree Black a) (Tree Black g)))))
-- p is red
greyColourFlip (Tree Red (Node p (Tree Grey g) (Tree Black (Node s (Tree Black l) (Tree c r))))) = (Tree Black (Node s (Tree Red (Node p (Tree Black g) (Tree Black l))) (Tree c r)))
greyColourFlip (Tree Red (Node p (Tree Black (Node s (Tree c r) (Tree Black l))) (Tree Grey g))) = (Tree Black (Node s (Tree c r) (Tree Red (Node p (Tree Black l) (Tree Black g)))))
-- r is red
greyColourFlip (Tree Black (Node p (Tree Grey g) (Tree Black (Node s (Tree Black l) (Tree Red r))))) = (Tree Black (Node s (Tree Black (Node p (Tree Black g) (Tree Black l))) (Tree Black r)))
greyColourFlip (Tree Black (Node p (Tree Black (Node s (Tree Red r) (Tree Black l))) (Tree Grey g))) = (Tree Black (Node s (Tree Black r) (Tree Black (Node p (Tree Black l) (Tree Black g)))))
-- s is red
greyColourFlip (Tree Black (Node p (Tree Grey g) (Tree Red (Node s (Tree Black l) (Tree Black r))))) = (Tree Black (Node s (Tree Red (Node p (Tree Grey g) (Tree Black l))) (Tree Black r)))
greyColourFlip (Tree Black (Node p (Tree Red (Node s (Tree Black r) (Tree Black l))) (Tree Grey g))) = (Tree Black (Node s (Tree Black r) (Tree Red (Node p (Tree Black l) (Tree Grey g)))))
-- else
greyColourFlip t = t

greyRebalance :: RBTree -> RBTree
greyRebalance t@(Tree c Leaf) = t
greyRebalance t = (Tree c (Node n (greyRebalance t1) (greyRebalance t2))) where
  (Tree c (Node n t1 t2)) = greyColourFlip t

delete :: Number -> RBTree -> RBTree
delete i t@(Tree c Leaf) = t
delete i (Tree c (Node n t1 t2@(Tree _ Leaf))) | n == i = t1
                                               | otherwise = (Tree c (Node n (delete i t1) t2))
delete i (Tree c (Node n t1 t2)) | n > i = (Tree c (Node n (delete i t1) t2))
                                 | n < i = (Tree c (Node n t1 (delete i t2)))
                                 | otherwise = greyRebalance(Tree c (Node (leftmostValue t2) t1 (removeLeftmostNode t2)))

aftekenBoom = Tree Black (Node 15
                        (Tree Black (Node 7
                            (Tree Red (Node 3
                                (Tree Black (Node 1
                                    (Tree Red (Node 1
                                        (Tree Black Leaf)
                                        (Tree Black Leaf)
                                    ))
                                    (Tree Red (Node 2
                                        (Tree Black Leaf)
                                        (Tree Black Leaf)
                                    ))
                                ))
                                (Tree Black (Node 5
                                    (Tree Red (Node 4
                                        (Tree Black Leaf)
                                        (Tree Black Leaf)
                                    ))
                                    (Tree Red (Node 6
                                        (Tree Black Leaf)
                                        (Tree Black Leaf)
                                    ))
                                ))
                            ))
                            (Tree Black (Node 10
                                (Tree Red (Node 8
                                    (Tree Black Leaf)
                                    (Tree Black Leaf)
                                ))
                                (Tree Red (Node 12
                                    (Tree Black Leaf)
                                    (Tree Black Leaf)
                                ))
                            ))
                        ))
                        (Tree Black (Node 25
                            (Tree Black (Node 20
                                (Tree Black Leaf)
                                (Tree Black Leaf)
                            ))
                            (Tree Red (Node 30
                                (Tree Black (Node 28
                                    (Tree Black Leaf)
                                    (Tree Black Leaf)
                                ))
                                (Tree Black (Node 60
                                    (Tree Black Leaf)
                                    (Tree Black Leaf)
                                ))
                            ))
                        ))
                    )
