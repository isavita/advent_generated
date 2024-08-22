import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"
    let total = sum $ map (decodeOutput . words) (lines contents)
    print total

decodeOutput :: [String] -> Int
decodeOutput input = 
    let patterns = take 10 input
        output = drop 11 input
        mapping = deduceMapping patterns
    in foldl (\acc o -> acc * 10 + fromJust (lookup (sort o) mapping)) 0 output

deduceMapping :: [String] -> [(String, Int)]
deduceMapping patterns = 
    let byLength n = filter (\p -> length p == n) patterns
        one = head $ byLength 2
        four = head $ byLength 4
        seven = head $ byLength 3
        eight = head $ byLength 7
        mapping = [ (sort one, 1), (sort four, 4), (sort seven, 7), (sort eight, 8) ]
        sixSegments = byLength 6
        fiveSegments = byLength 5
        zeroSixNine = map sort sixSegments
        twoThreeFive = map sort fiveSegments
        nine = head $ filter (\p -> all (`elem` p) four) zeroSixNine
        zero = head $ filter (\p -> all (`elem` p) seven && p /= nine) zeroSixNine
        six = head $ filter (\p -> p /= nine && p /= zero) zeroSixNine
        three = head $ filter (\p -> all (`elem` p) seven) twoThreeFive
        five = head $ filter (\p -> all (`elem` six) p) twoThreeFive
        two = head $ filter (\p -> p /= three && p /= five) twoThreeFive
    in mapping ++ [(zero, 0), (six, 6), (nine, 9), (two, 2), (three, 3), (five, 5)]