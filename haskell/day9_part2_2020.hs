
import System.IO

invalidNumber = 14360655

main = do
    contents <- readFile "input.txt"
    let numbers = map read $ lines contents :: [Int]
    let result = findContiguousSet numbers invalidNumber
    print result

findContiguousSet :: [Int] -> Int -> Int
findContiguousSet numbers target = head [minimum set + maximum set | i <- [0..length numbers - 1], let set = findSet i [] 0, sum set == target]
    where
        findSet i acc total
            | total == target = acc
            | total > target = []
            | otherwise = findSet (i + 1) (acc ++ [numbers !! i]) (total + numbers !! i)
