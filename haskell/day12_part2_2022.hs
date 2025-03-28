
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:<|)), (|>))
import Data.Char (ord)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import System.IO (readFile)

-- Type synonyms for clarity
type Point = (Int, Int) -- (row, col)
type Grid = Map.Map Point Char
type Visited = Set.Set Point
type Queue = Seq.Seq (Point, Int) -- ((row, col), distance)

-- Get elevation value, handling 'S' and 'E'
getElevation :: Char -> Int
getElevation 'S' = ord 'a'
getElevation 'E' = ord 'z'
getElevation c   = ord c

-- Find neighbors (up, down, left, right) that are within the grid
findNeighbors :: Grid -> Point -> [Point]
findNeighbors grid (r, c) =
    mapMaybe (\(dr, dc) ->
        let nextPos = (r + dr, c + dc)
        in if Map.member nextPos grid then Just nextPos else Nothing
    ) [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- Breadth-First Search implementation
-- Takes the grid, a check for valid moves, and a check for the target condition
bfs :: Grid -> (Grid -> Point -> Point -> Bool) -> (Grid -> Point -> Bool) -> Queue -> Visited -> Maybe Int
bfs grid isValidMove isTarget queue visited =
    case queue of
        Seq.Empty -> Nothing -- Target not reachable
        ((currentPos, dist) :<| restQueue) ->
            if isTarget grid currentPos then
                Just dist -- Target found
            else
                let -- Get neighbors on the grid
                    neighbors = findNeighbors grid currentPos
                    -- Filter neighbors based on the specific movement rule
                    validNeighbors = filter (isValidMove grid currentPos) neighbors
                    -- Filter out neighbors already visited
                    newNeighbors = filter (`Set.notMember` visited) validNeighbors
                    -- Add new neighbors to visited set
                    newVisited = foldr Set.insert visited newNeighbors
                    -- Add new neighbors to the queue
                    newQueue = foldl (\q n -> q |> (n, dist + 1)) restQueue newNeighbors
                in bfs grid isValidMove isTarget newQueue newVisited

-- --- Part 1 Specifics ---

-- Check if moving FROM 'fromPos' TO 'toPos' is valid for Part 1
-- Elevation of 'toPos' can be at most one higher than 'fromPos'
isValidMovePart1 :: Grid -> Point -> Point -> Bool
isValidMovePart1 grid fromPos toPos =
    maybe False (\toChar ->
        maybe False (\fromChar ->
            getElevation toChar <= getElevation fromChar + 1
        ) (Map.lookup fromPos grid)
    ) (Map.lookup toPos grid)

-- Target condition for Part 1: reaching the 'E' square
isTargetPart1 :: Grid -> Point -> Bool
isTargetPart1 grid pos = Map.lookup pos grid == Just 'E'

-- --- Part 2 Specifics ---

-- We run BFS backwards from 'E' for Part 2.
-- The check is reversed: can we move FROM 'fromPos' (closer to 'E') TO 'toPos' (further from 'E')?
-- This means the original move (toPos -> fromPos) must be valid.
-- elevation(fromPos) <= elevation(toPos) + 1
isValidMovePart2 :: Grid -> Point -> Point -> Bool
isValidMovePart2 grid fromPos toPos =
    maybe False (\fromChar ->
        maybe False (\toChar ->
            getElevation fromChar <= getElevation toChar + 1
        ) (Map.lookup toPos grid)
    ) (Map.lookup fromPos grid)

-- Target condition for Part 2 (reverse search): reaching any 'a' or 'S' square
isTargetPart2 :: Grid -> Point -> Bool
isTargetPart2 grid pos = maybe False (\c -> getElevation c == ord 'a') (Map.lookup pos grid)


-- --- Input Parsing ---

-- Parse the input string into the grid and find start/end points
parseInput :: String -> Maybe (Grid, Point, Point)
parseInput content =
    let ls = lines content
        indexedChars = [ ((r, c), char)
                       | (r, line) <- zip [0..] ls
                       , (c, char) <- zip [0..] line ]
        grid = Map.fromList indexedChars
        findPoint p = listToMaybe [ pos | (pos, char) <- indexedChars, char == p ]
    in do -- Use Maybe monad for safety
        startPos <- findPoint 'S'
        endPos   <- findPoint 'E'
        Just (grid, startPos, endPos)

-- --- Main Execution ---

main :: IO ()
main = do
    contents <- readFile "input.txt"
    case parseInput contents of
        Nothing -> putStrLn "Error parsing input or finding S/E."
        Just (grid, start, end) -> do
            -- Part 1: BFS from Start ('S') to End ('E')
            let initialQueue1 = Seq.singleton (start, 0)
            let initialVisited1 = Set.singleton start
            let result1 = bfs grid isValidMovePart1 isTargetPart1 initialQueue1 initialVisited1
            putStrLn $ "Part 1: " ++ maybe "No path found" show result1

            -- Part 2: Reverse BFS from End ('E') to any 'a'
            let initialQueue2 = Seq.singleton (end, 0)
            let initialVisited2 = Set.singleton end
            let result2 = bfs grid isValidMovePart2 isTargetPart2 initialQueue2 initialVisited2
            putStrLn $ "Part 2: " ++ maybe "No path found" show result2
