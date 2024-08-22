import Data.List
import Data.Char
import Data.Maybe
import System.IO

main = do
    contents <- readFile "input.txt"
    let heightmap = map (map digitToInt) $ lines contents
        basins = findBasins heightmap
        largestBasins = take 3 $ reverse $ sort $ map length basins
    print $ product largestBasins

findBasins :: [[Int]] -> [[(Int, Int)]]
findBasins heightmap = mapMaybe (\(x, y) -> findBasin heightmap x y) lowPoints
    where lowPoints = [(x, y) | x <- [0..length heightmap - 1], y <- [0..length (head heightmap) - 1], isLowPoint heightmap x y]

findBasin :: [[Int]] -> Int -> Int -> Maybe [(Int, Int)]
findBasin heightmap x y
    | heightmap !! x !! y == 9 = Nothing
    | otherwise = Just $ exploreBasin heightmap [] x y

exploreBasin :: [[Int]] -> [(Int, Int)] -> Int -> Int -> [(Int, Int)]
exploreBasin heightmap visited x y
    | (x, y) `elem` visited || heightmap !! x !! y == 9 = visited
    | otherwise = foldl' (\acc (nx, ny) -> exploreBasin heightmap acc nx ny) ((x, y) : visited) neighbors
    where neighbors = [(nx, ny) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)], let nx = x + dx, let ny = y + dy, inBounds heightmap nx ny]

isLowPoint :: [[Int]] -> Int -> Int -> Bool
isLowPoint heightmap x y = all (\(dx, dy) -> inBounds heightmap (x + dx) (y + dy) && heightmap !! x !! y < heightmap !! (x + dx) !! (y + dy)) neighbors
    where neighbors = [(-1, 0), (1, 0), (0, -1), (0, 1)]

inBounds :: [[Int]] -> Int -> Int -> Bool
inBounds heightmap x y = x >= 0 && x < length heightmap && y >= 0 && y < length (head heightmap)