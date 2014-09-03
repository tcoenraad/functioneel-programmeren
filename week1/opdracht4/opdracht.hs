import FPPrac

wortel1 :: Number -> Number -> Number -> String

wortel1 a b c | b^2 - 4*a*c < 0 = "discriminant negatief"
              | otherwise = show ((-b - sqrt(b^2 - 4*a*c))/(2*a)) 

			  
wortel2 :: Number -> Number -> Number -> String
			  
wortel2 a b c | b^2 - 4*a*c < 0 = "discriminant negatief"
              | otherwise = show ((-b + sqrt(b^2 - 4*a*c))/(2*a))