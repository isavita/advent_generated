
import Control.Monad (forM_)
import Data.List (nub)
import qualified Data.Set as Set

type Point = (Int, Int, Int, Int)

-- Calculate the Manhattan distance between two points
manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1, z1, w1) (x2, y2, z2, w2) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (w1 - w2)

-- Check if two points are in the same constellation
areConnected :: Point -> Point -> Bool
areConnected p1 p2 = manhattanDistance p1 p2 <= 3

-- Find all points connected to a given point using DFS
exploreConstellation :: Set.Set Point -> Point -> Set.Set Point
exploreConstellation points start = go Set.empty (Set.singleton start)
  where
    go visited toVisit
      | Set.null toVisit = visited
      | otherwise = let
          current = Set.findMin toVisit
          newVisited = Set.insert current visited
          newToVisit = Set.filter (`Set.notMember` newVisited) points
          connected = Set.filter (areConnected current) newToVisit
          remainingToVisit = Set.delete current toVisit
        in go newVisited (Set.union remainingToVisit connected)

-- Count the number of constellations
countConstellations :: Set.Set Point -> Int
countConstellations points = go points 0
  where
    go remaining count
      | Set.null remaining = count
      | otherwise = let
          point = Set.findMin remaining
          connectedPoints = exploreConstellation remaining point
          newRemaining = Set.difference remaining connectedPoints
        in go newRemaining (count + 1)

-- Read points from the input file
readPoints :: FilePath -> IO (Set.Set Point)
readPoints filePath = do
    contents <- readFile filePath
    let points = map parsePoint (lines contents)
    return (Set.fromList points)

-- Parse a point from a string
parsePoint :: String -> Point
parsePoint str = let [x, y, z, w] = map read (words (map (\c -> if c == ',' then ' ' else c) str))
                 in (x, y, z, w)

main :: IO ()
main = do
    points <- readPoints "input.txt"
    let constellations = countConstellations points
    print constellations
