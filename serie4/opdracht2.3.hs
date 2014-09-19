import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char
import Data.Either

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

