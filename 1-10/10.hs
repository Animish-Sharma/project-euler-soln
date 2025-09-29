sieve :: [Int] -> [Int]
sieve []     = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesBelow :: Int -> Int
primesBelow limit = sum $ sieve [2..limit-1]
