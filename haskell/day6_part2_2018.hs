
import Data.List
import System.IO

main = do
    contents <- readFile "input.txt"
    let coordinates = parseCoordinates $ lines contents
    let regionSize = findRegionSize coordinates 10000
    print regionSize

data Coordinate = Coordinate { x :: Int, y :: Int } deriving (Show)

parseCoordinates :: [String] -> [Coordinate]
parseCoordinates = map parseCoordinate

parseCoordinate :: String -> Coordinate
parseCoordinate str = let [x, y] = map read $ splitOn ", " str in Coordinate x y

findRegionSize :: [Coordinate] -> Int -> Int
findRegionSize coordinates maxDistance = length $ filter (< maxDistance) $ map (totalDistance coordinates) grid
    where
        grid = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
        (minX, minY, maxX, maxY) = findBoundingBox coordinates

totalDistance :: [Coordinate] -> (Int, Int) -> Int
totalDistance coordinates (x, y) = sum $ map (manhattanDistance x y) coordinates

findBoundingBox :: [Coordinate] -> (Int, Int, Int, Int)
findBoundingBox coordinates = (minimum xs, minimum ys, maximum xs, maximum ys)
    where
        xs = map x coordinates
        ys = map y coordinates

manhattanDistance :: Int -> Int -> Coordinate -> Int
manhattanDistance x y (Coordinate cx cy) = abs (x - cx) + abs (y - cy)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str = first : splitOn delim rest
    where
        (first, rest) = breakList delim str

breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList _ [] = ([], [])
breakList delim str
    | isPrefixOf delim str = ([], drop (length delim) str)
    | otherwise = let (token, rest) = breakList delim (tail str) in (head str : token, rest)
