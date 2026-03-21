import Data.List (nub)
import Data.Ratio ((%))

digits :: Int -> (Int, Int)
digits x = (x `div` 10, x `mod` 10)

isCurious :: Int -> Int -> Bool
isCurious n d
    | n >= d = False
    | n `mod` 10 == 0 && d `mod` 10 == 0 = False
    | otherwise = 
        let (n1, n2) = digits n
            (d1, d2) = digits d
        in or 
        [
            d2 /= 0 && n1 == d1 && (n2 % d2 == n % d),
            d1 /= 0 && n2 == d2 && (n1 % d1 == n % d),
            d1 /= 0 && n2 == d1 && (n1 % d2 == n % d),
            d2 /= 0 && n1 == d2 && (n2 % d1 == n % d)
        ]

fractions :: [(Int, Int)]
fractions = 
    [
        (n, d)
        | n <- [10 .. 99]
        , d <- [10 .. 99]
        , isCurious n d
    ]

result :: Integer
result =
  let prod = product [n % d | (n, d) <- fractions]
  in denominator prod

main :: IO ()
main = print result