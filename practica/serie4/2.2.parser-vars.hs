import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char
import Data.Either

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving Show

data S = E | O

parse :: S -> String -> (BinTree String (Either String Number), String)
parse E (x:xs) | x == '(' && head r3 == ')' = (Node op t1 t3, r4)
               | isDigit x = (Leaf (Right(read [x])), xs)
               | isLetter x = (Leaf (Left [x]), xs)
               | otherwise = error "Waa!"
        where 
        (t1,r1) = parse E xs
        ((Node op _ _), r2) = parse O r1
        (t3,r3) = parse E r2
        (_:r4) = r3

parse O (x:xs) | elem x "/^*+-" = (Node [x] (Leaf (Right 1)) (Leaf (Right 2)), xs)
               | otherwise = error "Woe!"
