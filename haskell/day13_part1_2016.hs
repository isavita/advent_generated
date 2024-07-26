
import Data.Bits (popCount)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Data.List (nub)

type Point = (Int, Int)

-- Function to determine if a point is a wall or open space
isOpenSpace :: Int -> Point -> Bool
isOpenSpace fav (x, y) = even $ popCount $ x*x + 3*x + 2*x*y + y + y*y + fav

-- Function to get neighbors of a point
neighbors :: Point -> [Point]
neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)], x + dx >= 0, y + dy >= 0]

-- BFS function to find the shortest path
bfs :: Int -> Point -> Point -> Maybe Int
bfs fav start goal = bfs' Set.empty [(start, 0)]
  where
    bfs' _ [] = Nothing
    bfs' visited ((current, steps):queue)
      | current == goal = Just steps
      | otherwise = bfs' newVisited (queue ++ newQueue)
      where
        newVisited = Set.insert current visited
        newQueue = [(n, steps + 1) | n <- neighbors current, isOpenSpace fav n, not (Set.member n visited)]

main :: IO ()
main = do
    -- Read the favorite number from input.txt
    fav <- read <$> readFile "input.txt"
    
    let start = (1, 1)
        goal = (31, 39)

    -- Find the shortest path
    let result = bfs fav start goal
    case result of
        Just steps -> print steps
        Nothing -> putStrLn "No path found"
