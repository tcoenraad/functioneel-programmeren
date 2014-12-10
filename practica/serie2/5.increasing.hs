import FPPrac

stijgend :: [Number] -> Bool
stijgend [x] = True
stijgend (x:xs) | x > head xs = False
                | otherwise = stijgend xs

zwakStijgend :: [Number] -> Bool
zwakStijgend [x] = True
zwakStijgend xs | last xs < average (take (length xs - 1) xs) = False
                | otherwise = zwakStijgend (take (length xs - 1) xs)

average :: [Number] -> Number
average xs = sum xs / fromIntegral (length xs)
