pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow b e = b * pow b (e - 1)

main :: IO ()
main = print (sum (map (read . (:[])) (show (pow 2 1000))))
