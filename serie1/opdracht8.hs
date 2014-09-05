import FPPrac

allEqual :: Eq(a) => [a] -> Bool
allEqual [a] = True
allEqual (x:xs) = x == head xs && allEqual xs

isRR :: [Number] -> Bool
isRR a = allEqual (calculateDifferences a)

calculateDifferences :: [Number] -> [Number]
calculateDifferences [a] = []
calculateDifferences (x:xs) = [x - head xs] ++ calculateDifferences xs
