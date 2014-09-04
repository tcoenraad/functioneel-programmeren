import FPPrac

r :: Number -> Number -> [Number]
r s v = [s] ++ r (s+v) v

r1 :: Number -> Number -> Number -> Number
r1 s v n = (r s v) !! n
