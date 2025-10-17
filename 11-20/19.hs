countSundats :: Integer -> Integer -> Integer
countSundats startYear endYear = fromIntegral $ length
    [ ()
    | year <- [startYear..endYear]
    , month <- [1..12]
    , let dayOfWeek = dayOfWeekOfFirst year month
    , dayOfWeek == 0  -- 0 represents Sunday
    ]
    where
        dayOfWeekOfFirst y m
            | m < 3     = zeller (y - 1) (m + 12) 1
            | otherwise = zeller y m 1

        zeller y m d =
            let k = y `mod` 100
                j = y `div` 100
            in (d + (13 * (m + 1)) `div` 5 + k + (k `div` 4) + (j `div` 4) + (5 * j)) `mod` 7
main :: IO ()
main = print $ countSundats 1901 2000