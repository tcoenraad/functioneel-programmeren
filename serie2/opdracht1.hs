import FPPrac

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs) | f x = x : myfilter f xs
                  | otherwise = myfilter f xs

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f s [] = s
myfoldl f s (x:xs) = myfoldl f a xs
  where a = f s x

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f s [] = s
myfoldr f s (x:xs) = f x (myfoldr f s xs)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] [] = []
myzipWith f (x:xs) (y:ys) = f x y : myzipWith f xs ys
