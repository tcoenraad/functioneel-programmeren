import FPPrac
import Data.List

isort :: [Number] -> [Number]
isort [n] = [n]
isort xs = minimum(xs) : isort (delete(minimum xs) xs)