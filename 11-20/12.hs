import Data.List (group)


primes :: [Integer]
primes = sieve[2..]
    where
        sieve(p:xs) = p : sieve[x | x <- xs, x `mod` p /= 0]
        sieve [] = []

primeFactors :: Integer -> [Integer]
primeFactors n = go n primes
    where
        go 1 _ = []
        go m (p:ps)
            | p * p > m = [m]
            | m `mod` p == 0 = p : go (m `div` p) (p:ps)
            | otherwise = go m ps
        go _ [] = error "Ran out of primes"


numDivisors :: Integer -> Integer
numDivisors 1 = 1
numDivisors n = product[ fromIntegral (length g + 1) | g <- group (primeFactors n)]

divisorsOfTriangle :: Integer -> Integer
divisorsOfTriangle n = 
    let (a, b) = if even n then (n `div` 2, n+1) else (n, (n + 1) `div` 2)
    in numDivisors a * numDivisors b

firstTriangularWithDivisors :: Integer -> Integer
firstTriangularWithDivisors limit =
  head [tri | n <- [1..], let tri = n * (n + 1) `div` 2, divisorsOfTriangle n > limit]



