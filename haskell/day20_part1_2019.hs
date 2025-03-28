
{-# LANGUAGE BangPatterns #-} -- Optional: Enforces strict evaluation which can sometimes improve performance

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:<|)), (|>)) -- Efficient queue operations
import Data.Char (isUpper)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>)) -- Useful for combining Maybe results
import System.IO (readFile)

-- Type aliases for better readability
type Coord = (Int, Int) -- Represents (Row, Col) coordinates
type Grid = M.Map Coord Char -- The maze layout stored as a map from coordinates to characters
type PortalMap = M.Map Coord Coord -- Maps a portal entrance '.' coordinate to its corresponding exit '.' coordinate

-- Data type used during parsing to determine portal label order
data Orientation = Vertical | Horizontal deriving (Eq, Show)

-- Parses the input file content into the necessary data structures:
-- Grid: The maze map.
-- Start Coordinate: The '.' adjacent to 'AA'.
-- End Coordinate: The '.' adjacent to 'ZZ'.
-- PortalMap: The mapping between paired portal '.' coordinates.
parseInput :: String -> (Grid, Coord, Coord, PortalMap)
parseInput input =
    let ls = lines input
        -- Create the grid map from input lines, ignoring space characters
        grid = M.fromList [ ((r, c), char)
                          | (r, line) <- zip [0..] ls
                          , (c, char) <- zip [0..] line
                          , char /= ' ' ]

        -- Identify all portal points. A portal point consists of:
        -- 1. The two-letter label (e.g., "BC").
        -- 2. The coordinate of the '.' tile adjacent to the portal letters.
        -- We iterate through the grid, looking for the *first* letter of a potential portal pair.
        portalPoints = mapMaybe (findPortalPoint grid) (M.toList grid)

        -- Group the identified portal points by their label.
        -- Result: Map String [Coord], e.g., "BC" -> [(r1,c1), (r2,c2)]
        portalInfo = M.fromListWith (++) [(label, [coord]) | (label, coord) <- portalPoints]

        -- Extract the unique start coordinate associated with the "AA" label.
        startCoord = case M.lookup "AA" portalInfo of
                         Just [c] -> c -- Expect exactly one '.' next to AA
                         _        -> error "Start portal AA not found or invalid (missing or >1 adjacent '.')"
        -- Extract the unique end coordinate associated with the "ZZ" label.
        endCoord   = case M.lookup "ZZ" portalInfo of
                         Just [c] -> c -- Expect exactly one '.' next to ZZ
                         _        -> error "End portal ZZ not found or invalid (missing or >1 adjacent '.')"

        -- Build the final PortalMap for teleportation.
        -- This map connects the two '.' coordinates for each portal pair (excluding AA and ZZ).
        -- For a portal labeled "BC" with adjacent '.' at c1 and c2, we add mappings c1 -> c2 and c2 -> c1.
        portalMap = M.foldrWithKey buildPortalConnections M.empty (M.delete "AA" $ M.delete "ZZ" portalInfo)

    in (grid, startCoord, endCoord, portalMap)

-- Helper function for parsing:
-- Given a grid cell (coordinate, character), checks if it's the first letter of a portal.
-- If yes, it finds the second letter, determines the correct label based on position,
-- and finds the coordinate of the '.' tile adjacent to the portal.
-- Returns Maybe (LabelString, DotCoordinate).
findPortalPoint :: Grid -> (Coord, Char) -> Maybe (String, Coord)
findPortalPoint grid (coord@(r, c), char1)
    | not (isUpper char1) = Nothing -- Skip if the character is not an uppercase letter
    | otherwise = -- Check downwards for a vertical portal, then rightwards for a horizontal portal
          -- <|> allows combining the results: if the first check fails (Nothing), the second is tried.
          checkNeighbor Vertical   (r + 1, c) (r - 1, c) (r + 2, c) -- Check down
      <|> checkNeighbor Horizontal (r, c + 1) (r, c - 1) (r, c + 2) -- Check right
  where
    -- Checks if the neighborCoord contains the second letter of a portal.
    -- dotGuessOuter/Inner are the potential coordinates of the adjacent '.' tile.
    checkNeighbor :: Orientation -> Coord -> Coord -> Coord -> Maybe (String, Coord)
    checkNeighbor orientation neighborCoord dotGuessOuter dotGuessInner =
        case M.lookup neighborCoord grid of
            Just char2 | isUpper char2 -> -- Found the second letter!
                -- Determine the label string. Order matters (Top->Bottom, Left->Right).
                -- Since we only check Downwards (Vertical) and Rightwards (Horizontal)
                -- from the first character, the order [char1, char2] is correct.
                let label = [char1, char2]
                -- Find the actual '.' coordinate adjacent to this portal pair.
                -- It could be at dotGuessOuter or dotGuessInner.
                -- (>>=) is the bind operator for Maybe: if findAdjacentDot succeeds, apply the function.
                in findAdjacentDot grid dotGuessOuter dotGuessInner >>= (\dot -> Just (label, dot))
            _ -> Nothing -- The neighbor is not an uppercase letter.

-- Finds the '.' tile adjacent to a portal, given two potential locations.
-- One location is "outside" the first letter, the other "outside" the second letter.
findAdjacentDot :: Grid -> Coord -> Coord -> Maybe Coord
findAdjacentDot g dotGuess1 dotGuess2 =
    case M.lookup dotGuess1 g of
        Just '.' -> Just dotGuess1 -- Found '.' at the first location
        _        -> case M.lookup dotGuess2 g of
                      Just '.' -> Just dotGuess2 -- Found '.' at the second location
                      _        -> Nothing       -- No adjacent '.' found (indicates invalid input)

-- Helper function to build the final PortalMap used for teleportation.
-- Takes a portal label (String), its list of adjacent '.' coordinates ([Coord]),
-- and the PortalMap being built. Adds bidirectional mappings for portal pairs.
buildPortalConnections :: String -> [Coord] -> PortalMap -> PortalMap
-- Expect exactly two coordinates for paired portals.
buildPortalConnections _ [c1, c2] !pm = M.insert c1 c2 (M.insert c2 c1 pm) -- Add c1->c2 and c2->c1 mappings strictly
-- Error handling for invalid portal definitions (e.g., label with != 2 points)
buildPortalConnections label coords _ = error $ "Invalid portal definition for " ++ label ++ ": expected 2 coords, found " ++ show (length coords) ++ " for " ++ show coords


-- Implements Breadth-First Search (BFS) to find the shortest path length.
-- Takes the grid, portal map, start, and end coordinates. Returns the minimum steps.
bfs :: Grid -> PortalMap -> Coord -> Coord -> Int
bfs grid portals start end = go (Seq.singleton (start, 0)) (S.singleton start)
  where
    -- The main recursive BFS loop.
    -- queue: A sequence (used as a FIFO queue) of states (Coordinate, Distance).
    -- visited: A set of coordinates already visited to prevent cycles and redundant work.
    go :: Seq (Coord, Int) -> S.Set Coord -> Int
    go queue visited
      -- Base case: If the queue is empty, the end was not reachable.
      | Seq.null queue = error "End coordinate not reachable"
      | otherwise =
          -- Dequeue the element with the smallest distance currently in the queue.
          -- Use BangPatterns (!) for strictness, potentially improving performance by avoiding thunks.
          let ((!currentCoord, !dist) :<| restQueue) = queue
          in -- Goal check: If the current coordinate is the destination, return the distance.
             if currentCoord == end
             then dist
             else
               -- Explore neighbors: Find all coordinates reachable in one step.
               let neighbors = findValidNeighbors grid portals currentCoord
                   -- Process the neighbors: Filter out already visited ones,
                   -- add the new ones to the queue (with distance + 1) and the visited set.
                   -- foldr processes the neighbors list to update the queue and visited set.
                   (newQueue, newVisited) = foldr (enqueue dist visited) (restQueue, visited) neighbors
               in go newQueue newVisited -- Recursively call `go` with the updated queue and visited set.

    -- Finds all valid coordinates reachable in one step from the current coordinate.
    -- This includes adjacent '.' tiles and the destination of a portal if standing on one.
    findValidNeighbors :: Grid -> PortalMap -> Coord -> [Coord]
    findValidNeighbors g ps coord@(r, c) =
        -- 1. Find orthogonal neighbors (Up, Down, Left, Right).
        let adjacentCoords = [ (r+1, c), (r-1, c), (r, c+1), (r, c-1) ]
            -- Filter these neighbors to keep only those that are open passages ('.').
            validMoves = filter (\adj -> M.lookup adj g == Just '.') adjacentCoords
        -- 2. Check if the current coordinate is a portal entrance (i.e., in the PortalMap).
        in case M.lookup coord ps of
            Just portalExitCoord -> portalExitCoord : validMoves -- If yes, add the portal's exit coordinate to the list of neighbors.
            Nothing              -> validMoves                   -- If no, just return the list of adjacent open passages.

    -- Helper function used within the foldr in `go`.
    -- Adds a neighbor to the queue and visited set if it hasn't been visited yet.
    enqueue :: Int -> S.Set Coord -> Coord -> (Seq (Coord, Int), S.Set Coord) -> (Seq (Coord, Int), S.Set Coord)
    enqueue !currentDist visitedSet neighbor (!currentQueue, !currentVisited)
        -- Check if the neighbor has already been visited.
        | S.member neighbor currentVisited = (currentQueue, currentVisited) -- If yes, do nothing.
        -- If not visited, add it to the end of the queue (with incremented distance)
        -- and add it to the visited set.
        | otherwise = (currentQueue |> (neighbor, currentDist + 1), S.insert neighbor currentVisited)


-- Main function: Reads input from "input.txt", parses it, runs BFS, and prints the result.
main :: IO ()
main = do
    -- Read the maze layout from the input file.
    input <- readFile "input.txt"
    -- Parse the input string into the grid, start/end coordinates, and portal map.
    let (grid, startCoord, endCoord, portalMap) = parseInput input
    -- Perform BFS to find the shortest path length from start to end.
    let shortestPath = bfs grid portalMap startCoord endCoord
    -- Print the result to standard output.
    print shortestPath
