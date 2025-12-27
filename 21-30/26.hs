import Data.List (maximumBy)
import Data.Ord (comparing)


cycleLength :: Int -> Int
cycleLength d = go 1 [] 0
    where
        go r seen len
            | r == 0 = 0
            | r `elem` seen = len - index r seen
            | otherwise = go((r * 10) `mod` d) (seen ++ [r]) (len + 1)
        index x xs = length (takeWhile (/= x) xs)    

main :: IO()
main = print result
    where 
        result = fst $
            maximumBy (comparing snd)
            [(d, cycleLength d) | d <- [1..999]]