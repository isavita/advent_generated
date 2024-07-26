
import qualified Data.Set as Set
import Data.Bits (popCount)

favoriteNumber :: Int
favoriteNumber = 1362

data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord)

isWall :: Int -> Int -> Bool
isWall x y = odd $ popCount num
  where num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber

bfsMaxSteps :: Point -> Int -> Int
bfsMaxSteps start maxSteps = go Set.empty [start] 0
  where
    go visited [] _ = Set.size visited
    go visited queue steps
      | steps >= maxSteps = Set.size visited
      | otherwise = go newVisited newQueue (steps + 1)
      where
        (newVisited, newQueue) = foldl expand (visited, []) queue
        expand (v, q) point = foldl addPoint (v, q) deltas
          where
            deltas = [Point (x point + dx) (y point + dy) | (dx, dy) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]]
            addPoint (v, q) next
              | x next < 0 || y next < 0 || isWall (x next) (y next) || Set.member next v = (v, q)
              | otherwise = (Set.insert next v, next:q)

main :: IO ()
main = print $ bfsMaxSteps (Point 1 1) 50
