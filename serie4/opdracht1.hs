import FPPrac.Trees.ParseTree
import FPPrac

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b)

--data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

type Tree1a = BinTree Number Number
--exampleTree1a = Node1a 1 (Leaf1a 35) (Leaf1a 36) 

--data Tree1b = Leaf1b (Number, Number) | Node1b (Number, Number) Tree1b Tree1b

type Tree1b = BinTree (Number, Number) (Number, Number)

--data Tree1c = Leaf1c | Node1c Number Tree1c Tree1c

type Tree1c = BinTree Number Unit

data Unit = U

instance Show Unit where
  show U = ""

pp :: (Show a, Show b) => BinTree a b -> ParseTree
pp (Node a t1 t2) = ParseNode (show a) [(pp t1), (pp t2)]
pp (Leaf b) = ParseNode (show b) []

--showTree(pp(Node 1 (Leaf 2) (Leaf 3))) 