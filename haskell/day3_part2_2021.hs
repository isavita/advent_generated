import System.IO

main = do
    contents <- readFile "input.txt"
    let input = lines contents
        oxygenRating = findRating input 0 mostCommon
        co2Rating = findRating input 0 leastCommon
        lifeSupportRating = binaryToDecimal oxygenRating * binaryToDecimal co2Rating
    print lifeSupportRating

findRating :: [String] -> Int -> ([String] -> Int -> Char) -> String
findRating [number] _ _ = number
findRating numbers position criteria = 
    let bitCriteria = criteria numbers position
        filteredNumbers = filter (\num -> num !! position == bitCriteria) numbers
    in findRating filteredNumbers (position + 1) criteria

mostCommon :: [String] -> Int -> Char
mostCommon numbers position = 
    let count1s = length $ filter (\num -> num !! position == '1') numbers
        count0s = length numbers - count1s
    in if count1s >= count0s then '1' else '0'

leastCommon :: [String] -> Int -> Char
leastCommon numbers position = 
    let count1s = length $ filter (\num -> num !! position == '1') numbers
        count0s = length numbers - count1s
    in if count1s < count0s then '1' else '0'

binaryToDecimal :: String -> Int
binaryToDecimal binary = foldl (\acc bit -> acc * 2 + if bit == '1' then 1 else 0) 0 binary