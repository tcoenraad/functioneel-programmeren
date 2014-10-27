data OA = Add | Mul
data OB = EQu | GTh | LTh
data ExprA = Const Int
           | Var Char
           | OpA OA ExprA ExprA
           | If ExprB ExprA ExprA

data ExprB = OpB OB ExprA ExprA

evalA :: ExprA -> Int
evalA (Const a) = a
evalA (OpA Add expr1 expr2) = (evalA expr1) + (evalA expr2)
evalA (OpA Mul expr1 expr2) = (evalA expr1) * (evalA expr2)
evalA (If exprB expr1 expr2) | (evalB exprB) = evalA expr1
                             | otherwise = evalA expr2

evalB :: ExprB -> Bool
evalB (OpB EQu expr1 expr2) = evalA expr1 == evalA expr2
evalB (OpB GTh expr1 expr2) = evalA expr1 > evalA expr2
evalB (OpB LTh expr1 expr2) = evalA expr1 < evalA expr2
