import Data.List

nthPerm :: Int -> [Int] -> [Int]

nthPerm 0 xs = xs
nthPerm n xs = pick n xs
  where
    pick _ [] = []
    pick k ys =
      let f = factorial (length ys - 1)
          (q, r) = k `divMod` f
          y = ys !! q
      in y : pick r (delete y ys)

factorial :: Int -> Int
factorial 0 = 1
factorial n = product [1..n]

main :: IO()

main =
    putStrLn $ concatMap show $ nthPerm 999999 [0..9]
    