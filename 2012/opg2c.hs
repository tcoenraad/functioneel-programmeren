module Exercise where

data OA = Add | Mul deriving Show
data OB = EQu | GTh | LTh deriving Show
data ExprA = Const Int
           | Var Char
           | OpA OA ExprA ExprA
           | If ExprB ExprA ExprA
            deriving Show

data ExprB = OpB OB ExprA ExprA deriving Show

toNF :: ExprA -> ExprA
toNF (OpA Add (OpA Add expr1 expr2) (expr3)) = OpA Add (toNF expr1) (OpA Add (toNF expr2) (toNF expr3))
toNF (OpA Mul (OpA Mul expr1 expr2) (expr3)) = OpA Mul (toNF expr1) (OpA Mul (toNF expr2) (toNF expr3))
toNF (OpA Mul expr1 (OpA Add expr2 expr3)) = OpA Add (OpA Mul (toNF expr1) (toNF expr2)) (OpA Mul (toNF expr1) (toNF expr3))
toNF expr = expr
