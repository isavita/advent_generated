
main = do
    contents <- readFile "input.txt"
    let validTriangles = length $ filter isValidTriangle $ map parseInput $ lines contents
    print validTriangles

parseInput :: String -> (Int, Int, Int)
parseInput line = case map read $ words line of
    [a, b, c] -> (a, b, c)
    _ -> error "Invalid input format"

isValidTriangle :: (Int, Int, Int) -> Bool
isValidTriangle (a, b, c) = a + b > c && a + c > b && b + c > a
