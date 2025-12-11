import Data.List
import Data.Char

nameValue :: String -> Int
nameValue = sum . map (\c -> ord c - ord 'A' + 1)

main :: IO ()
main = do
    contents <- readFile "names.txt"
    let names = sort (read ("[" ++ contents ++ "]") :: [String])
    print $ sum [nameValue n * i | (i, n) <- zip [1..] names]
