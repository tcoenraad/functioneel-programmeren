import FPPrac

r :: Number -> Number -> [Number]
r s v = [s] ++ r (s+v) v
