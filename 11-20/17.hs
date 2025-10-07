import Data.Char (isAlpha)
import Data.ByteString (count)

toWords :: Int -> String
toWords n 
    | n == 1000 = "one thousand"
    | n >= 100  = hundredsPart n
    | n >= 20   = tensPart n
    | otherwise = units !! n
    where
        units = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                 "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                 "sixteen", "seventeen", "eighteen", "nineteen"]
        tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
        tensPart x
            | x < 20 = units !! x
            | r == 0 = tens !! (x `div` 10)
            | otherwise = tens !! (x `div` 10) ++ "-" ++ units !! r
            where r = x `mod` 10

        hundredsPart x =
            let h = x `div` 100
                r = x `mod` 100
                base = units !! h ++ " hundred"
            in if r == 0
                then base
                else base ++ " and " ++ toWords r
        

countWords :: String -> Int
countWords = length . filter isAlpha

main :: IO ()
main = print $ sum $ map (countWords . toWords) [1..1000]