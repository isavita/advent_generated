
import Data.List
import System.IO

main = do
    contents <- readFile "input.txt"
    print $ firstNUnique contents 14

firstNUnique :: String -> Int -> Int
firstNUnique s n = case findIndex (\x -> length x == length (nub x)) (map (take n) (tails s)) of
    Just idx -> idx + n
    Nothing -> -1
