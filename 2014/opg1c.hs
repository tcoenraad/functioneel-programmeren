module Exercise where

pascal :: Int -> [[Int]]
pascal n = [(pascal' k) | k <- [0..n]] 

pascal' :: Int -> [Int]
pascal' 0 = [1]
pascal' 1 = [1, 1]
pascal' n = [1] ++ pascal'' (pascal' (n-1)) ++ [1]

pascal'' :: [Int] -> [Int]
pascal'' n = zipWith (+) n (tail n)
