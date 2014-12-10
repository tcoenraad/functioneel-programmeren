import FPPrac

extrX :: Number -> Number -> Number -> Number
extrX a b c = (-b) / (2*a)

extrY :: Number -> Number -> Number -> Number
extrY a b c = a * x ^ 2 + b * x + c
  where x = extrX a b c