module Exercise where

money :: Int -> [[Int]]
money 0 = [[]]
money n = [(k:rest) | k <- [1,2,5,10,20,50],
                      k <= n,
                      rest <- (money (n - k)),
                      k <= minimum (n:rest), -- ordering does not matter
                      n == sum (k:rest)
                      ]
