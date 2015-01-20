module Exercise where

list35 :: Int -> [Int]
list35 k = [x | x <- [1..k-1], x `mod` 3 == 0 || x `mod` 5 == 0]

add35 :: Int -> Int
add35 k = sum $ list35 k

add :: [Int] -> Int -> Int
add ks n = sum $ filter (\x -> any (\y -> x `mod` y == 0) ks) [0..n-1]

-- alternative
add' :: [Int] -> Int -> Int
add' ks n = sum [n | n <- [0..n-1], any (\k -> n`mod`k == 0) ks]
