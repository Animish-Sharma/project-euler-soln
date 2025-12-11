import Data.List (nub)

d :: Int -> Int
d n = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]

isAmicable :: Int -> Bool
isAmicable a =
    let b = d a
    in a /= b && d b == a


main :: IO()

main = print $ sum (filter isAmicable [1 .. 9999])