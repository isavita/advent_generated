
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let numbers = map read $ lines contents :: [Int]
        count = length $ filter (\(x, y) -> y > x) $ zip numbers (drop 1 numbers)
    print count
