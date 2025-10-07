fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)

num :: Integer
num = fact 40

main :: IO ()
main = print (num `div` (fact 20 * fact 20))