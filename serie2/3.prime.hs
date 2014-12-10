import FPPrac

zeef :: [Number] -> [Number]
zeef [] = []
zeef (x:xs) = x : zeef (filter (\n ->n `mod` x /= 0) xs)

isPriem :: Number -> Bool
isPriem n = n == last (zeef [2..n])

eersteNPriems :: Number -> [Number]
eersteNPriems n = take n (zeef [2..])

eerstePriemsTotN :: Number -> [Number]
eerstePriemsTotN n = zeef [2..n]

delers :: Number -> [Number]
delers n = filter (\x -> n `mod` x == 0) [1..n]

isPriem2 :: Number -> Bool
isPriem2 n = length (delers n) == 2
