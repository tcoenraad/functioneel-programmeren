module Exercise where

type Item a = (a, Int)
type Bag a = [Item a]

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag sub bag = and $ map (\(a, n) -> (countItem a bag) >= n) sub

getItem :: Eq a => a -> Bag a -> Item a
getItem item [] = (item, 0)
getItem item (it@(a, _):bag) | a == item = it
                             | otherwise = getItem item bag

countItem :: Eq a => a -> Bag a -> Int
countItem item bag = n where
  (_, n) = getItem item bag

union :: Eq a => Bag a -> Bag a -> Bag a
union as bs = map (\(a, v) -> (a, v + (countItem a bs))) as ++ complement as bs

complement :: Eq a => Bag a -> Bag a -> Bag a
complement as bs = filter (\(a, _) -> (countItem a as) == 0) bs
