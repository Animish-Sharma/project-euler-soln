fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = print result
    where
        result = length (takeWhile (< 10^(999)) fibs) + 1