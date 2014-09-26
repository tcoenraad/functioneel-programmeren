import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving Show

data S = E | O

parse :: S -> String -> (BinTree Char Number, String)
parse E (x:xs) | x == '(' && r3 == ')' = (Node op t1 t3, rs3)
               | isDigit x = (Leaf (read [x]), xs)
               | otherwise = error "Waa!"
        where 
        (t1, r1) = parse E xs
        ((Node op _ _), r2) = parse O r1
        (t3, r3:rs3) = parse E r2

parse O (x:xs) | elem x "/^*+-" = (Node x (Leaf 1) (Leaf 2), xs)
               | otherwise = error "Woe!"
