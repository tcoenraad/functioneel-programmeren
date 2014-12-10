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

myconcat :: [[n]] -> [n]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: [Number] -> Number
mymaximum [n] = n
mymaximum [] = error "undefined"
mymaximum (x:xs) | x > mymaximum xs = x
                 | otherwise = mymaximum xs

myzip :: [n] -> [n] -> [(n, n)]
myzip [] [] = []
myzip [] [n] = []
myzip [n] [] = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys
