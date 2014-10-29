import Data.List

powerset :: Ord a => [a] -> [[a]]
-- powerset :: [Int] -> [[Int]]
powerset set = [elem:set | elem <- set, set <- []:powerset(filter ((<) elem) (set \\ [elem]))]
