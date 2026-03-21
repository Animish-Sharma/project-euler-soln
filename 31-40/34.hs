import Data.Char (digitToInt)

fact :: [Int]
fact = scanl (*) 1 [1 .. 9]

factDigit :: Int -> Int
factDigit d = fact !! d

sumFactDigits :: Int -> Int
sumFactDigits n = sum . map factDigit . map digitToInt . show $ n

result :: Int
result = sum [n | n <- [1 .. 999999], n == sumFactDigits n]

main :: IO()
main = print (result - 3)