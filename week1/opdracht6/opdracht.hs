import FPPrac

mylength :: [Number] -> Number
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mysum :: [Number] -> Number
mysum [] = 0
mysum (x:xs) = x + mysum(xs)

myreverse :: [Number] -> [Number]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake :: Number -> [Number] -> [Number]
mytake 0 (x:xs) = []
mytake n [] = []
mytake n (x:xs) = [x] ++ mytake (n-1) xs
