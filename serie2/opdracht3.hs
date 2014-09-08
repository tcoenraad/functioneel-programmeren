import FPPrac
import Data.List
import Data.Char

a = [2..40]
alleNatuurlijkeGetallen = [2..]

zeef :: [Number] -> [Number]
zeef [] = []
zeef (x:xs) = x : zeef (filter (\n ->n`mod`x/=0) xs)



-- [2..40] -> [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]


