import FPPrac

eqlLarge :: [[n]] -> Bool
eqlLarge [n] = True
eqlLarge (x:xs) = length x == length(head xs) && eqlLarge xs

sumRows :: [[Number]] -> [Number]
sumRows [] = []
sumRows (x:xs) = [sum x] ++ sumRows xs

transpose :: [[n]] -> [[n]]
transpose ([]:_) = []
transpose x = [map head x] ++ transpose (map tail x)

sumColumns :: [[Number]] -> [Number]
sumColumns x = sumRows (transpose x)
