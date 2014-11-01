module Exercise where

type Matrix = [[Int]]
type MatrixM = [[Matrix]]

sumSubMatrices :: MatrixM -> Matrix
sumSubMatrices mats = map (\x -> map (sum.map sum) x) mats
