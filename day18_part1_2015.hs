
import Data.List
import System.IO

gridSize = 100
steps = 100

countOnNeighbors :: [[Bool]] -> Int -> Int -> Int
countOnNeighbors grid x y = length $ filter id [grid !! nx !! ny | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0, let nx = x + dx, let ny = y + dy, nx >= 0, nx < gridSize, ny >= 0, ny < gridSize]

step :: [[Bool]] -> [[Bool]]
step grid = [[newVal x y | y <- [0..gridSize-1]] | x <- [0..gridSize-1]]
    where newVal x y
            | grid !! x !! y && onNeighbors == 2 || onNeighbors == 3 = True
            | not (grid !! x !! y) && onNeighbors == 3 = True
            | otherwise = False
            where onNeighbors = countOnNeighbors grid x y

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let grid = [[c == '#' | c <- line] | line <- lines contents]

    let finalGrid = iterate step grid !! steps

    let onCount = length $ filter id $ concat finalGrid

    print onCount
