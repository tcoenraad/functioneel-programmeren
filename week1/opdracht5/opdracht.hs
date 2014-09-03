import FPPrac

extrX :: Number -> Number -> Number -> Number
extrX a b c = (-b) / (2*a)

extrY :: Number -> Number -> Number -> Number
extrY a b c = a*(extrX a b c)^2 + b*(extrX a b c) + c 