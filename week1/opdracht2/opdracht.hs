import FPPrac

codeer :: Char -> Char
codeer c | ord(c) <= ord('z') && ord(c) >= ord('a') = chr (((ord c - ord('a') + 3) `mod` 26) + ord('a'))
         | ord(c) <= ord('Z') && ord(c) >= ord('A') = chr (((ord c - ord('A') + 3) `mod` 26) + ord('A'))
         | otherwise = c
