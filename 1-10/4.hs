isPalindrome :: Int -> Bool
isPalindrome n = let s = show n in s == reverse s -- represents n in string using show then check if s == s

products :: Int
products = maximum $ filter isPalindrome [x * y | x <- [999, 998 .. 100], y <- [999,998 .. 100] ]