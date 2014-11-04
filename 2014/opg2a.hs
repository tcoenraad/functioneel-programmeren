module Exercise where

bijnaGelijk :: Ord a => [a] -> [a] -> Bool
bijnaGelijk as bs | length as /= length bs = False
                  | otherwise = 1 >= length (filter (\(a, b) -> a /= b) (zip as bs))
