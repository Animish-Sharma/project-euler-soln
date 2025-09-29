
digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

windows :: Int -> [a] -> [[a]]
windows k xs
  | length xs < k = []
  | otherwise     = take k xs : windows k (tail xs)

windowProducts :: Int -> Integer -> Integer
windowProducts k n = maximum $ map product (windows k (digits n))
