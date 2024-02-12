
import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"
    let ids = lines contents
        charCounts = map countChars ids
        twoCount = length $ filter hasTwos charCounts
        threeCount = length $ filter hasThrees charCounts
        checksum = twoCount * threeCount
    print checksum

countChars :: String -> [(Char, Int)]
countChars = map (\x -> (head x, length x)) . group . sort

hasTwos :: [(Char, Int)] -> Bool
hasTwos = isJust . find (\(_, count) -> count == 2)

hasThrees :: [(Char, Int)] -> Bool
hasThrees = isJust . find (\(_, count) -> count == 3)
