import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let grid = lines contents
        visibleTrees = countVisibleTrees grid
    print visibleTrees

countVisibleTrees :: [String] -> Int
countVisibleTrees grid = length $ filter (isVisible grid) [(x, y) | x <- [0..length grid - 1], y <- [0..length (head grid) - 1]]

isVisible :: [String] -> (Int, Int) -> Bool
isVisible grid (x, y) = isEdge grid x y || any (allShorter grid (x, y)) directions
    where
        directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

isEdge :: [String] -> Int -> Int -> Bool
isEdge grid x y = x == 0 || y == 0 || x == length grid - 1 || y == length (head grid) - 1

allShorter :: [String] -> (Int, Int) -> (Int, Int) -> Bool
allShorter grid (x, y) (dx, dy) = all (\(nx, ny) -> grid !! nx !! ny < grid !! x !! y) $ takeWhile inBounds $ iterate (\(nx, ny) -> (nx + dx, ny + dy)) (x + dx, y + dy)
    where
        inBounds (nx, ny) = nx >= 0 && ny >= 0 && nx < length grid && ny < length (head grid)