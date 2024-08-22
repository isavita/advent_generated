import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let grid = map (map (== '#')) $ lines contents
    let gridSize = length grid
    let fixedCorners = [(0, 0), (0, gridSize - 1), (gridSize - 1, 0), (gridSize - 1, gridSize - 1)]
    let finalGrid = iterate (step gridSize fixedCorners) (setFixedCorners grid fixedCorners) !! 100
    print $ countLightsOn finalGrid

step :: Int -> [(Int, Int)] -> [[Bool]] -> [[Bool]]
step gridSize fixedCorners grid = setFixedCorners newGrid fixedCorners
  where
    newGrid = [[nextState x y | y <- [0..gridSize-1]] | x <- [0..gridSize-1]]
    nextState x y = let onNeighbors = countOnNeighbors gridSize grid x y
                    in if grid !! x !! y
                       then onNeighbors == 2 || onNeighbors == 3
                       else onNeighbors == 3

countOnNeighbors :: Int -> [[Bool]] -> Int -> Int -> Int
countOnNeighbors gridSize grid x y = length [() | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0),
                                                   let nx = x + dx, let ny = y + dy,
                                                   nx >= 0, nx < gridSize, ny >= 0, ny < gridSize,
                                                   grid !! nx !! ny]

setFixedCorners :: [[Bool]] -> [(Int, Int)] -> [[Bool]]
setFixedCorners grid fixedCorners = foldl (\g (x, y) -> updateGrid g x y True) grid fixedCorners

updateGrid :: [[Bool]] -> Int -> Int -> Bool -> [[Bool]]
updateGrid grid x y val = take x grid ++ [take y (grid !! x) ++ [val] ++ drop (y + 1) (grid !! x)] ++ drop (x + 1) grid

countLightsOn :: [[Bool]] -> Int
countLightsOn grid = sum $ map (length . filter id) grid