module Exercise where

-- recursive
jollyJumper :: [Int] -> Bool
jollyJumper n = jollyJumper' ((length n) - 1) n

jollyJumper' :: Int -> [Int] -> Bool
jollyJumper' _ [_] = True
jollyJumper' size (x:y:xs) | abs (x - y) == size = jollyJumper' (size - 1) (y:xs)
                           | otherwise = False

-- higher-order
jollyJump :: [Int] -> Bool
jollyJump xs = and $ zipWith (==) diff (tail diff) where
  diff = difference $ difference xs

difference :: [Int] -> [Int]
difference [_] = []
difference (x:y:xs) = abs(x - y):difference(y:xs)
