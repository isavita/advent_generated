
import System.IO

main = do
    contents <- readFile "input.txt"
    let forest = lines contents
    let trees = countTrees forest 3 1
    print trees

countTrees :: [String] -> Int -> Int -> Int
countTrees forest right down = length $ filter (\(y, x) -> forest !! y !! (x `mod` width) == '#') coordinates
    where
        width = length (head forest)
        coordinates = zip [0, down..length forest - 1] [0, right..]

