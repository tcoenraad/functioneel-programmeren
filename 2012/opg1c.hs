module Exercise where

import Data.List

-- recursive
matrixMultiplying :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultiplying [] [] = []
matrixMultiplying as bs = matrixMultiplying' as (transpose bs)

matrixMultiplying' :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultiplying' [] _ = []
matrixMultiplying' (a:as) b = calcRow a b : matrixMultiplying' as b

calcRow :: [Int] -> [[Int]] -> [Int]
calcRow _ [] = []
calcRow a (b:bs) = sum (zipWith (*) a b) : calcRow a bs

-- higher order
matrixMultiply :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultiply as bs = map (\a -> map (\b -> sum (zipWith (*) a b)) (transpose bs)) as

-- list comprehension
matrixMultiplier :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultiplier a b = [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]
