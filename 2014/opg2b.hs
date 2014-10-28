subset :: Ord a => [a] -> [a] -> Bool
subset sub set = and $ map (\x -> x `elem` set) sub
