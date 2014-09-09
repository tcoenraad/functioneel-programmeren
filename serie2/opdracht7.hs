import FPPrac

bubble :: [Number] -> [Number]
bubble [n] = [n]
bubble (x:y:xs) | x > y = [y] ++ bubble (x:xs)
                | otherwise = [x] ++ bubble (y:xs)
				
bsort :: [Number] -> [Number]
bsort [] = []
bsort xs = bsort(take (length(xs)-1) (bubble(xs))) ++ [last(bubble xs)]