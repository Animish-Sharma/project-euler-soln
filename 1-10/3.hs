largestPrimeFactor :: Int -> Int
largestPrimeFactor x = helper x 2
    where
        helper m d
            | d * d > m = m
            | m `mod` d == 0 = helper (m `div` d) d
            | otherwise = helper m (d + 1)