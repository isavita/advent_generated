
import Data.List (minimumBy)
import Data.Set (Set, empty, fromList, insert, member)
import qualified Data.Set as Set

type Point = (Int, Int)
type Grid = Set Point

-- Function to parse a line of input into a Point
parsePoint :: String -> Point
parsePoint line =
  let [x, y] = map read $ wordsWhen (== ',') line
   in (x, y)

-- Helper function to split a string based on a delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- Function to get neighbors of a point within grid bounds
neighbors :: Int -> Int -> Point -> [Point]
neighbors maxX maxY (x, y) =
  filter
    (\(nx, ny) -> nx >= 0 && nx <= maxX && ny >= 0 && ny <= maxY)
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Function to perform breadth-first search to find shortest path
bfs :: Int -> Int -> Point -> Point -> Grid -> Maybe Int
bfs maxX maxY start end grid = bfs' [(start, 0)] Set.empty
  where
    bfs' [] _ = Nothing
    bfs' ((p, d) : q) visited
      | p == end = Just d
      | p `Set.member` visited || p `Set.member` grid = bfs' q visited
      | otherwise =
          bfs'
            (q ++ map (, d + 1) (neighbors maxX maxY p))
            (Set.insert p visited)

-- Function to find the first byte that blocks the path
findBlockingByte :: Int -> Int -> [Point] -> Point
findBlockingByte maxX maxY points =
  head
    [ p
      | (p, i) <- zip points [1 ..],
        isNothing (bfs maxX maxY (0, 0) (maxX, maxY) (fromList $ take i points))
    ]
  where
    isNothing Nothing = True
    isNothing _ = False

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let points = map parsePoint $ lines contents
  let maxX = 70
  let maxY = 70
  let start = (0, 0)
  let end = (maxX, maxY)

  -- Part 1: Find shortest path after 1024 bytes
  let grid1024 = fromList $ take 1024 points
  let shortestPath = bfs maxX maxY start end grid1024
  case shortestPath of
    Just d -> putStrLn $ "Shortest path after 1024 bytes: " ++ show d
    Nothing -> putStrLn "No path found after 1024 bytes"

  -- Part 2: Find the first byte that blocks the path
  let blockingByte = findBlockingByte maxX maxY points
  putStrLn $
    "First blocking byte: "
      ++ show (fst blockingByte)
      ++ ","
      ++ show (snd blockingByte)
