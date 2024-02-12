
import System.IO

main = do
    contents <- readFile "input.txt"
    let offsets = map read $ lines contents
    print $ solveMaze offsets 0 0

solveMaze :: [Int] -> Int -> Int -> Int
solveMaze offsets index steps
    | index < 0 || index >= length offsets = steps
    | otherwise = solveMaze newOffsets newIndex (steps + 1)
    where jump = offsets !! index
          newOffsets = incrementOffset offsets index
          newIndex = index + jump

incrementOffset :: [Int] -> Int -> [Int]
incrementOffset offsets index = 
    take index offsets ++ [offsets !! index + 1] ++ drop (index + 1) offsets
