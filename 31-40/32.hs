import Data.List (nub, sort)

isPandigital :: String -> Bool
isPandigital s = sort s == "123456789"

pandigitalProducts :: [Int]
pandigitalProducts = 
    [ p
    | a <- [1 .. 9],
    b <- [1234 .. 9876],
    let p = a * b,
    isPandigital (show a ++ show b ++ show p)
    ] ++ 
    [ p
    | a <- [12 .. 98],
    b <- [123 .. 987],
    let p = a * b,
    isPandigital (show a ++ show b ++ show p)
    ]

main :: IO()
main = print . sum . nub $ pandigitalProducts