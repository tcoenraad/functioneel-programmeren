import FPPrac
import Data.List

isort :: [Number] -> [Number]
isort x = foldl (insert1) [] x 

insert1 :: [Number] -> Number -> [Number]
insert1 [] a = [a]
insert1 (x:xs) a | a<=x = a : (x:xs)
                 | otherwise = x: insert1 xs a
