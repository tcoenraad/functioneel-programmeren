import FPPrac

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = let (as, bs) = splitAt (length xs `quot` 2) xs
           in merge (mSort as) (mSort bs)
