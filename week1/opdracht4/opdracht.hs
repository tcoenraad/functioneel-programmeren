import FPPrac

discr :: Number -> Number -> Number -> Number
discr a b c = b^2 - 4*a*c

wortel1 :: Number -> Number -> Number -> Number

wortel1 a b c | discr a b c < 0 = error "discriminant negatief"
              | otherwise = (-b - sqrt(discr a b c))/(2*a)

			  
wortel2 :: Number -> Number -> Number -> Number
			  
wortel2 a b c | discr a b c < 0 = error "discriminant negatief"
              | otherwise = (-b + sqrt(discr a b c))/(2*a)