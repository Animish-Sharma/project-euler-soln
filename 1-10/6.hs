diff :: Integer -> Integer
diff x = (sum1 ^ 2) - sum2
    where
        sum1 = (x * (x+1)) `div` 2
        sum2 = (x * (x+1) * (2*x + 1)) `div` 6