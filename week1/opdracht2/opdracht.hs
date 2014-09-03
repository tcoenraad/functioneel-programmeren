import FPPrac

codeer :: (Char, Number) -> Char
codeer (c, shift) | ord(c) <= ord('z') && ord(c) >= ord('a') = chr (((ord c - ord('a') + shift) `mod` 26) + ord('a'))
                  | ord(c) <= ord('Z') && ord(c) >= ord('A') = chr (((ord c - ord('A') + shift) `mod` 26) + ord('A'))
                  | otherwise = c
