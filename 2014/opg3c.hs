module Exercise where

data Tree = Node Int Tree Tree | Leaf Int

histogram :: Tree -> [(Int, Int)]
histogram t = filter (\(_, n) -> n /= 0) (histogram' t)

histogram' :: Tree -> [(Int, Int)]
histogram' t = [(a, count t a) | a <- [zoekMin t..zoekMax t]] where

count :: Tree -> Int -> Int
count t n = length $ filter (\x -> x == n) (allValues t)

allValues :: Tree -> [Int]
allValues (Node i t1 t2) = [i] ++ allValues t1 ++ allValues t2
allValues (Leaf i) = [i]

zoekMin :: Tree -> Int
zoekMin t = minimum $ allValues t

zoekMax :: Tree -> Int
zoekMax t = maximum $ allValues t
