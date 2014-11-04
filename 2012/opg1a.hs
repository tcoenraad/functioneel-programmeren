module Exercise where

perfect :: Int -> Bool
perfect n = sum (delers n) == n

delers :: Int -> [Int]
delers n = [x | x <- [1..n-1], n `mod` x == 0]

perfectTill :: Int -> [Int]
perfectTill n = filter perfect [1..n]

