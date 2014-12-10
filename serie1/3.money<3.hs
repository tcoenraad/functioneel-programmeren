import FPPrac

geld :: Number -> Number -> Number -> Number
geld bedrag percentage 0 = bedrag
geld bedrag percentage jaren = ((percentage/100) + 1) * geld bedrag percentage (jaren-1)

-- geld 300 4 10
