import FPPrac
import RoseTree

data Tree1c = Leaf1c | Node1c Number Tree1c Tree1c

pp1c :: Tree1c -> RoseTree
pp1c (Node1c i t1 t2) = RoseNode (show i) [(pp1c t1), (pp1c t2)]
pp1c Leaf1c = RoseNode "" []

t_insert :: Number -> Tree1c -> Tree1c
t_insert n (Node1c i t1 t2) | i >= n = Node1c i (t_insert n t1) t2
                            | otherwise = Node1c i t1 (t_insert n t2)
t_insert n (Leaf1c) = Node1c n Leaf1c Leaf1c

--showTree(pp1c(t_insert 5 (Node1c 5 (Leaf1c ) (Leaf1c ))))

makeTreeRec :: [Number] -> Tree1c
makeTreeRec xs = makeTreeRec' xs Leaf1c
makeTreeRec' (x:xs) tree = makeTreeRec' xs (t_insert x tree)
makeTreeRec' [] tree = tree

-- flip for reversing arguments
makeTreeFold :: [Number] -> Tree1c
makeTreeFold xs = foldl (flip t_insert) Leaf1c xs

makeList :: Tree1c -> [Number]
makeList (Node1c i t1 t2) =  (makeList t1) ++ [i] ++ (makeList t2)
makeList (Leaf1c) = []

makeSortedList :: [Number] -> [Number]
makeSortedList xs = makeList (makeTreeFold xs)

makeSortedTree :: Tree1c -> Tree1c
makeSortedTree xs = makeTreeFold (makeList xs)
