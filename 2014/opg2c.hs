import Data.List

powerset :: Ord a => [a] -> [[a]]
powerset set = [elem:set | elem <- set, set <- []:powerset(filter ((<) elem) (set \\ [elem]))]
