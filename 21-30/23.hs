import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

limit :: Int
limit = 28123

sumProperDivs :: UArray Int Int
sumProperDivs = runSTUArray $ do
  a <- newArray (1, limit) 0 :: ST s (STUArray s Int Int)
  forM_ [1 .. limit `div` 2] $ \d ->
    forM_ [2*d,3*d .. limit] $ \m -> do
      v <- readArray a m
      writeArray a m (v + d)
  return a

abundants :: [Int]
abundants = [n | n <- [1..limit], sumProperDivs ! n > n]

canBeSum :: UArray Int Bool
canBeSum = runSTUArray $ do
  a <- newArray (1, limit) False :: ST s (STUArray s Int Bool)
  forM_ abundants $ \x ->
    forM_ abundants $ \y -> 
      let s = x + y in
      when (s <= limit) $ writeArray a s True
  return a

main :: IO ()
main = print $ sum [n | n <- [1..limit], not (canBeSum ! n)]
