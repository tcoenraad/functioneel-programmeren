module Exercise where

type Matrix = [[Int]]
type MatrixM = [[Matrix]]

splits :: Matrix -> Int -> Int -> MatrixM
splits mat m n = map (splitsHorizontally m) matrices where
  matrices = splitsVertically n mat

splitsHorizontally :: Int -> Matrix -> [Matrix]
splitsHorizontally m mat | length (head mat) <= m = [mat]
                         | otherwise = map (take m) mat : splitsHorizontally m (map (drop m) mat)

splitsVertically :: Int -> Matrix -> [Matrix]
splitsVertically n mat | length mat <= n = [mat]
                       | otherwise = (take n mat) : splitsVertically n (drop n mat)
