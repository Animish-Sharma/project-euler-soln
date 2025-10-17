fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

sumOfDigits :: Integer -> Integer
sumOfDigits 0 = 0
sumOfDigits n = (n `mod` 10) + sumOfDigits (n `div` 10)

main :: IO ()
main = print $ sumOfDigits (fact 100)