import FPPrac

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs) | f x = x : myfilter f xs
                  | otherwise = myfilter f xs

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f s [n] = f s n
myfoldl f s (x:xs) = myfoldl f a xs
  where a = f s x

myfoldr :: (b -> a -> b) -> b -> [a] -> b
myfoldr f s [n] = f s n
myfoldr f s x = myfoldr f a b
  where a = f s (last x)
        b = take (length x - 1) x

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] [] = []
myzipWith f (x:xs) (y:ys) = f x y : myzipWith f xs ys
