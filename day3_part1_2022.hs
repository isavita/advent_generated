
import Data.Char
import Data.List

itemPriority :: Char -> Int
itemPriority item
    | item >= 'a' && item <= 'z' = ord item - ord 'a' + 1
    | otherwise = ord item - ord 'A' + 27

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let totalSum = sum [itemPriority c | line <- lines contents, let half = length line `div` 2, let firstCompartment = take half line, let secondCompartment = drop half line, c <- nub firstCompartment, c `elem` secondCompartment]
    print totalSum
