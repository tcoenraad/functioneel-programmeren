import FPPrac

qsort :: [Number] -> [Number]
qsort (x:xs) | minimum (x:xs) == x = (x:xs)
             | otherwise = qsort(filter (<x) (x:xs) ++ filter (==x) (x:xs) ++ filter (>x) (x:xs))