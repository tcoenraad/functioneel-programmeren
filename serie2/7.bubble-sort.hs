import FPPrac

bubble :: [Number] -> [Number]
bubble [n] = [n]
bubble (x:y:xs) | x > y = [y] ++ bubble (x:xs)
                | otherwise = [x] ++ bubble (y:xs)
				
bsort :: [Number] -> [Number]
bsort [] = []
bsort xs | xs == xs_bubbled_once = xs
         | otherwise = bsort (init xs_bubbled_once) ++ [last xs_bubbled_once]
            where xs_bubbled_once = bubble(xs)
