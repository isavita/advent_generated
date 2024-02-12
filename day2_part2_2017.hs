
import System.IO

main = do
    contents <- readFile "input.txt"
    let sums = map (solveLine . map read . words) (lines contents)
    print $ sum sums

solveLine :: [Int] -> Int
solveLine nums = sum [num1 `div` num2 | num1 <- nums, num2 <- nums, num1 `mod` num2 == 0, num1 /= num2]
