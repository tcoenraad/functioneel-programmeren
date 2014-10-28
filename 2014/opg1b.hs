money :: Int -> [[Int]]
money n = [(k:rest) | k <- [1,2,5,10,20,50],
                      (n - k) >= 0,
                      rest <- []:(money (n - k)),
                      if not (null rest) then k <= head rest else True,
                      (k + sum rest) == n]
