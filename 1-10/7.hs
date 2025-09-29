isPrime :: Int -> Bool

isPrime n
    | n < 2 = False
    | otherwise = null [x | x <- [2 .. issqrt n], n `mod` x == 0]
    where
        issqrt :: Int -> Int
        issqrt = floor . sqrt . fromIntegral

primes :: [Int]
primes = filter isPrime [2 ..]
