import Data.List (maximumBy)
import Data.Ord (comparing)

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | even n    = False
  | otherwise = null [x | x <- [3,5..isqrt n], n `mod` x == 0]
  where
    isqrt = floor . sqrt . fromIntegral

countPrimes :: Int -> Int -> Int -> Int
countPrimes a b n
  | val <= 1      = 0
  | isPrime val   = 1 + countPrimes a b (n + 1)
  | otherwise     = 0
  where
    val = n*n + a*n + b

coefficients :: [(Int, Int)]
coefficients =
  [ (a, b)
  | a <- [-999, -997 .. 999]  
  , b <- [2 .. 1000]
  , isPrime b
  ]

bestPair :: (Int, Int)
bestPair =
  maximumBy
    (comparing (\(a,b) -> countPrimes a b 0))
    coefficients

main :: IO ()
main = print $ uncurry (*) bestPair
