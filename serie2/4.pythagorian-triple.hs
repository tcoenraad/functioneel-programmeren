import FPPrac

pyth :: Number -> [(Number, Number, Number)]
pyth n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], (a^2)+(b^2) == (c^2)]

pyth2 :: Number -> [(Number, Number, Number)]
pyth2 n = [(a, b, c) | a <- [1..n], b <- [(a+1)..n], c <- [(b+1)..n], (a^2)+(b^2) == (c^2) && gcd a b == 1]

-- a < b < c
-- a, b relatively prime
