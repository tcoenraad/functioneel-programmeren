import FPPrac

allEqual :: Eq(a) => [a] -> Bool
allEqual [a] = True
allEqual (x:xs) = x == head xs && allEqual xs
