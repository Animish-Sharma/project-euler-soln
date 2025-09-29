fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib(x - 1) + fib(x - 2)

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


evenFib :: Int -> Int
evenFib limit = sum $ filter even $ takeWhile (<limit) fibs