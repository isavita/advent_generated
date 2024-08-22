import qualified Data.Map as Map
import System.IO

data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord)

main = do
    contents <- readFile "input.txt"
    let linesInput = lines contents
        points = concatMap parseLine linesInput
        overlaps = length $ filter (\(_, count) -> count >= 2) $ Map.toList $ foldl countPoints Map.empty points
    print overlaps

parseLine :: String -> [Point]
parseLine line = case words line of
    [start, "->", end] -> let (x1, y1) = parsePoint start
                              (x2, y2) = parsePoint end
                          in if x1 == x2 || y1 == y2
                             then generatePoints x1 y1 x2 y2
                             else generateDiagonalPoints x1 y1 x2 y2
    _ -> []

parsePoint :: String -> (Int, Int)
parsePoint pointStr = let (xStr, yStr) = break (== ',') pointStr
                      in (read xStr, read (tail yStr))

generatePoints :: Int -> Int -> Int -> Int -> [Point]
generatePoints x1 y1 x2 y2
    | x1 == x2 = [Point x1 y | y <- [min y1 y2 .. max y1 y2]]
    | otherwise = [Point x y1 | x <- [min x1 x2 .. max x1 x2]]

generateDiagonalPoints :: Int -> Int -> Int -> Int -> [Point]
generateDiagonalPoints x1 y1 x2 y2
    | x1 < x2 && y1 < y2 = [Point x y | (x, y) <- zip [x1..x2] [y1..y2]]
    | x1 < x2 && y1 > y2 = [Point x y | (x, y) <- zip [x1..x2] [y1,y1-1..y2]]
    | x1 > x2 && y1 < y2 = [Point x y | (x, y) <- zip [x1,x1-1..x2] [y1..y2]]
    | otherwise = [Point x y | (x, y) <- zip [x1,x1-1..x2] [y1,y1-1..y2]]

countPoints :: Map.Map Point Int -> Point -> Map.Map Point Int
countPoints acc point = Map.insertWith (+) point 1 acc