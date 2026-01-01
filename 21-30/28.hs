diagonalSum :: Int -> Int
diagonalSum n = 
    1 + sum[4 * k * k - 6 * (k - 1) | k <- [3, 5 .. n] ]


main :: IO()
main = print $ diagonalSum 1001