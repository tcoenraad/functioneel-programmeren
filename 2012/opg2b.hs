module Exercise where

data OA = Add | Mul
data OB = EQu | GTh | LTh
data ExprA = Const Int
           | Var Char
           | OpA OA ExprA ExprA
           | If ExprB ExprA ExprA

data ExprB = OpB OB ExprA ExprA

evalA :: ExprA -> [(Char, Int)] -> (Int, [(Char, Int)])
evalA (Const a) dict = (a, dict)
evalA (Var a) dict = (b, dict)
  where b = findVar a dict
evalA (OpA Add expr1 expr2) dict = (fst (evalA expr1 dict) + fst (evalA expr2 dict), dict)
evalA (OpA Mul expr1 expr2) dict = (fst (evalA expr1 dict) * fst (evalA expr2 dict), dict)
evalA (If exprB expr1 expr2) dict | (evalB exprB dict) = (fst (evalA expr1 dict), dict)
                                  | otherwise = (fst (evalA expr2 dict), dict)

findVar :: Char -> [(Char, Int)] -> Int
findVar _ [] = error "Var not found!"
findVar a ((c, d):dict) | a == c = d
                        | otherwise =  findVar a dict

evalB :: ExprB -> [(Char, Int)] -> Bool
evalB (OpB EQu expr1 expr2) dict = evalA expr1 dict == evalA expr2 dict
evalB (OpB GTh expr1 expr2) dict = evalA expr1 dict > evalA expr2 dict
evalB (OpB LTh expr1 expr2) dict = evalA expr1 dict < evalA expr2 dict
