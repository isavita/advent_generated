
main = do
    contents <- readFile "input.txt"
    let numbers = map (map read . words) $ lines contents
    let validTriangles = length [() | i <- [0..length (head numbers) - 1], j <- [0,3..length numbers - 1], j+2 < length numbers, isValidTriangle (numbers !! j !! i) (numbers !! (j+1) !! i) (numbers !! (j+2) !! i)]
    print validTriangles

isValidTriangle a b c = a + b > c && a + c > b && b + c > a
