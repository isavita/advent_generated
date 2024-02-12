
import System.IO

countCombinations :: [Int] -> Int -> Int -> Int
countCombinations containers target index
    | target == 0 = 1
    | target < 0 || index >= length containers = 0
    | otherwise = countCombinations containers (target - containers !! index) (index + 1) +
                  countCombinations containers target (index + 1)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let containers = map read (lines contents) :: [Int]
    print $ countCombinations containers 150 0
    hClose handle
