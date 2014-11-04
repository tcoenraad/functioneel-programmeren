module Exercise where

data Tree = Node Int [Tree] | Leaf Int deriving Show

subboom :: [Int] -> Tree -> Tree
subboom [] t = t
subboom _ (Leaf _) = error "Path too deep"
subboom (p:path) (Node _ ts) | p < length ts = subboom path (ts !! p)
                             | otherwise = error "Index too large"
