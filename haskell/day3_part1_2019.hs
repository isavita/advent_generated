import qualified Data.Set as Set
import System.IO (readFile)

type Point = (Int, Int)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let [wire1, wire2] = lines contents
        path1 = splitOn ',' wire1
        path2 = splitOn ',' wire2
        points1 = tracePath path1
        points2 = tracePath path2
        intersections = Set.intersection (Set.fromList points1) (Set.fromList points2)
        closestDistance = minimum $ map manhattanDistance $ Set.toList intersections
    print closestDistance

tracePath :: [String] -> [Point]
tracePath moves = go (0, 0) moves []
  where
    go _ [] acc = acc
    go (x, y) (m:ms) acc = let (dir:distStr) = m
                               dist = read distStr
                               newPoints = case dir of
                                   'R' -> [(x+i, y) | i <- [1..dist]]
                                   'L' -> [(x-i, y) | i <- [1..dist]]
                                   'U' -> [(x, y+i) | i <- [1..dist]]
                                   'D' -> [(x, y-i) | i <- [1..dist]]
                           in go (last newPoints) ms (acc ++ newPoints)

manhattanDistance :: Point -> Int
manhattanDistance (x, y) = abs x + abs y

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delimiter str = let (word, rest) = break (== delimiter) str
                        in word : case rest of
                                    [] -> []
                                    (_:xs) -> splitOn delimiter xs