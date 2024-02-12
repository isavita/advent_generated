
import Data.List
import Data.Char

main = do
    contents <- readFile "input.txt"
    let seatIDs = sort $ map (decode . map replaceChar) (lines contents)
    print $ findMissingSeat seatIDs

replaceChar :: Char -> Char
replaceChar 'F' = '0'
replaceChar 'B' = '1'
replaceChar 'L' = '0'
replaceChar 'R' = '1'

decode :: String -> Int
decode pass = row * 8 + column
    where row = binaryToInt (take 7 pass)
          column = binaryToInt (drop 7 pass)

binaryToInt :: String -> Int
binaryToInt binaryStr = foldl' (\acc x -> acc * 2 + digitToInt x) 0 binaryStr

findMissingSeat :: [Int] -> Int
findMissingSeat (x:y:xs)
    | y /= x + 1 = x + 1
    | otherwise = findMissingSeat (y:xs)
