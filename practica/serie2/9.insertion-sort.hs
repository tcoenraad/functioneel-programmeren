import FPPrac
import Data.List

isort :: [Number] -> [Number]
isort x = foldl (flip insert) [] x 
