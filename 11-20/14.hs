import Data.Array

limit :: Int
limit = 1000000
nextCollatz :: Int -> Int
nextCollatz n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1
collatzLengths :: Array Int Int
collatzLengths = listArray (1, limit) [collatzLen n | n <- [1..limit]]
  where
    collatzLen 1 = 1
    collatzLen n
      | next <= limit = 1 + collatzLengths ! next
      | otherwise     = 1 + collatzLen next
      where next = nextCollatz n

main :: IO ()
main = do
  let (num, len) = maximum [(n, collatzLengths ! n) | n <- [1..limit]]
  putStrLn $ show num
