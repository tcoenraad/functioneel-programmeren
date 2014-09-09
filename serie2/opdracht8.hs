import FPPrac
import Data.List

minmax :: Ord a => [a] -> [a]
minmax [] = []
minmax xs = [min] ++ minmax ((\\) ((\\) xs [min]) [max]) ++ [max]
  where min = minimum xs
        max = maximum xs
