import FPPrac

mylength :: [n] -> Number
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mysum :: [Number] -> Number
mysum [] = 0
mysum (x:xs) = x + mysum(xs)

myreverse :: [n] -> [n]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake :: Number -> [n] -> [n]
mytake 0 (x:xs) = []
mytake n [] = []
mytake n (x:xs) = [x] ++ mytake (n-1) xs

myelem :: (Eq n) => n -> [n] -> Bool
myelem n [] = False
myelem n (x:xs) = (x == n) || (myelem n xs)
