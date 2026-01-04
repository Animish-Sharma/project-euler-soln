coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

target :: Int
target = 200

main :: IO ()
main = print ways

ways :: Int
ways = last $ foldl update (1 : replicate target 0) coins
  where
    update dp coin =
      [ if i < coin
          then dp !! i
          else dp !! i + dp !! (i - coin)
        | i <- [0 .. target]
      ]
