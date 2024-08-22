import System.IO

main = do
    contents <- readFile "input.txt"
    let count = sum [length $ filter (\x -> length x `elem` [2, 3, 4, 7]) (words $ dropWhile (/= '|') line) | line <- lines contents]
    print count