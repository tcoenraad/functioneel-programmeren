import FPPrac

deellijst :: Eq a => [a] -> [a] -> Bool
deellijst x [] = False
deellijst x y = zip x y == zip x x || deellijst x (tail y)

sublijst :: Eq a => [a] -> [a] -> Bool
sublijst [] _ = True
sublijst xs [] = False
sublijst (x:xs) ys = (elem x ys) && sublijst xs (tail (dropWhile (/=x) ys))

--deellijst [1,2,3] [0,1,2,3,4] -> True
--deellijst [1,2,3] [0,3,2,1,4] -> False

--sublijst[1,2,3] [0,1,33,2,3] -> True
--sublijst[1,2,3] [0,1,33,3,2] -> False
