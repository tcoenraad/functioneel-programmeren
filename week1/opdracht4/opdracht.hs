import FPPrac

discr :: Number -> Number -> Number -> Number
discr a b c = b^2 - 4*a*c

wortel1 :: Number -> Number -> Number -> String

wortel1 a b c | discr a b c < 0 = "discriminant negatief"
              | otherwise = show ((-b - sqrt(discr a b c))/(2*a)) 

			  
wortel2 :: Number -> Number -> Number -> String
			  
wortel2 a b c | discr a b c < 0 = "discriminant negatief"
              | otherwise = show ((-b + sqrt(discr a b c))/(2*a))