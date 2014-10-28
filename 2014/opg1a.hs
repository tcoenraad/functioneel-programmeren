list35 :: Int -> [Int]
list35 k = [x | x <- [1..k-1], x `mod` 3 == 0 || x `mod` 5 == 0]

add35 :: Int -> Int
add35 k = sum $ list35 k
