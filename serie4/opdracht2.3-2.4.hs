import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char
import Data.Either

pp :: (Show a, Show b) => BinTree a b -> ParseTree
pp (Node a t1 t2) = ParseNode (show a) [(pp t1), (pp t2)]
pp (Leaf b) = ParseNode (show b) []

data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving Show

data ADT = LP | RP | OP String | NUM Number | VAR String deriving Show

tokenizer :: String -> [ADT]
tokenizer [] = []
tokenizer (x:xs) | x == ' ' = tokenizer xs
                 | elem x "/+*-^" = OP [x] : tokenizer xs
                 | x == '(' = LP : tokenizer xs
                 | x == ')' = RP : tokenizer xs
                 | isDigit x = (NUM (read number :: Number)) : tokenizer remainder1
                 | isAlpha x = (VAR word) : tokenizer remainder2
                   where
                     (number, remainder1) = getNumber(x:xs)
                     (word, remainder2) = getWord(x:xs)

getNumber :: String -> (String, String)
getNumber x = (takeWhile isDigit x, dropWhile isDigit x)

getWord :: String -> (String, String)
getWord x = (takeWhile isAlpha x, dropWhile isAlpha x)

parseString :: String -> (BinTree String (Either String Number))
parseString x | length r == 0 = p
              | otherwise = error "Too long string"
              where (p, r) = parse E (tokenizer x)

data S = E | O

parse :: S -> [ADT] -> (BinTree String (Either String Number), [ADT])
parse E (LP:xs) = (Node op t1 t3, r4)
                where 
                (t1,r1) = parse E xs
                ((Node op _ _), r2) = parse O r1
                (t3,r3) = parse E r2
                r4 = consume r3
parse E ((NUM x):xs) = (Leaf (Right x), xs)
parse E ((VAR x):xs) = (Leaf (Left x), xs)
parse E (x:xs) = error "Waa!"
parse O ((OP x):xs) = (Node x (Leaf (Right 1)) (Leaf (Right 2)), xs)
parse O (x:xs) = error "Wee!"

consume :: [ADT] -> [ADT]
consume (RP:xs) = xs
consume [] = error "Missing parenthesis"

eval :: (BinTree String (Either String Number)) -> Number
eval (Node "+" t1 t2) = (eval t1) + (eval t2)
eval (Node "-" t1 t2) = (eval t1) - (eval t2)
eval (Node "*" t1 t2) = (eval t1) * (eval t2)
eval (Node "/" t1 t2) = (eval t1) / (eval t2)
eval (Node "^" t1 t2) = (eval t1) ^ (eval t2)
eval (Leaf l) = n where
  (Right n) = l
