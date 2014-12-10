-- http://stackoverflow.com/questions/11168238/haskell-generating-all-paths-between-nodes

import Prelude

g!1 = [2,6]
g!2 = [3,5]
g!3 = [4]
g!4 = [5]
g!5 = []
g!6 = []

paths x y g
    | x == y    = [[]]
    | otherwise = [(x,t):path | t <- g!x, path <- paths t y g]
