import FPPrac

qsort :: [Number] -> [Number]
qsort [] = []
qsort (x:xs) = qsort(filter (<x) (x:xs)) ++ filter (==x) (x:xs) ++ qsort(filter (>x) (x:xs))
