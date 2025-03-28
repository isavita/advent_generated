
{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import System.IO (readFile)
import Control.Monad (guard)
import Data.List (maximum)

-- Type aliases for clarity
type Coord = (Int, Int)
type Grid = M.Map Coord Char
type Visited = S.Set Coord
-- Graph representation for Part 2: Node -> (Neighbor -> Distance)
type Graph = M.Map Coord (M.Map Coord Int)

-- --- Input Parsing ---

-- Parses the input string into a grid, dimensions, start, and end coordinates.
-- Only stores walkable tiles ('.', '^', '>', 'v', '<') in the map for efficiency.
parseInput :: String -> (Grid, Int, Int, Coord, Coord)
parseInput input =
    let ls = lines input
        rows = length ls
        cols = length (head ls)
        -- Create a map of (row, col) -> char for walkable tiles only
        grid = M.fromList [ ((r, c), char)
                          | (r, rowStr) <- zip [0..] ls
                          , (c, char) <- zip [0..] rowStr
                          , char /= '#'
                          ]
        -- Find the single '.' in the first row
        start = fst $ head $ M.toList $ M.filterWithKey (\(r, _) _ -> r == 0) grid
        -- Find the single '.' in the last row
        end = fst $ head $ M.toList $ M.filterWithKey (\(r, _) _ -> r == rows - 1) grid
    in (grid, rows, cols, start, end)

-- --- Part 1: Longest Path with Slopes ---

-- Finds neighbors for Part 1, respecting slope directions.
getNeighborsPart1 :: Grid -> Coord -> [Coord]
getNeighborsPart1 grid current@(r, c) = fromMaybe [] $ do
    tile <- M.lookup current grid
    let potentialNeighbors = case tile of
          '^' -> [(r - 1, c)]
          '>' -> [(r, c + 1)]
          'v' -> [(r + 1, c)]
          '<' -> [(r, c - 1)]
          '.' -> [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
          _   -> [] -- Should not happen as '#' are filtered out
    -- Filter neighbors that are within the grid (i.e., exist in the map)
    pure $ filter (`M.member` grid) potentialNeighbors

-- Depth-first search to find the longest path for Part 1.
dfsPart1 :: Grid -> Coord -> Coord -> Visited -> Int -> Maybe Int
dfsPart1 grid current end visited len
    | current == end = Just len -- Reached the end
    | otherwise =
        let visited' = S.insert current visited
            -- Find valid next steps (respecting slopes and not visited)
            nextSteps = filter (`S.notMember` visited') (getNeighborsPart1 grid current)
            -- Recursively explore paths from valid next steps
            pathLengths = mapMaybe (\next -> dfsPart1 grid next end visited' (len + 1)) nextSteps
        in if null pathLengths
           then Nothing -- Dead end
           else Just $ maximum pathLengths -- Return the max length found from this point

solvePart1 :: Grid -> Coord -> Coord -> Int
solvePart1 grid start end = fromMaybe 0 $ dfsPart1 grid start end S.empty 0

-- --- Part 2: Longest Path ignoring Slopes (Graph Condensation) ---

-- Get all four potential neighbors (up, down, left, right)
getAllNeighbors :: Coord -> [Coord]
getAllNeighbors (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- Identify junctions: points with more than 2 neighbors (plus start/end)
findJunctions :: Grid -> Coord -> Coord -> S.Set Coord
findJunctions grid start end = S.insert start $ S.insert end $ M.keysSet $ M.filterWithKey isJunction grid
  where
    isJunction pos _ =
        let neighbors = filter (`M.member` grid) (getAllNeighbors pos)
        in length neighbors > 2

-- Build a condensed graph where nodes are junctions and edges are path lengths between them.
buildCondensedGraph :: Grid -> S.Set Coord -> Graph
buildCondensedGraph grid junctions = M.fromListWith M.union $ concatMap findEdgesFrom (S.toList junctions)
  where
    findEdgesFrom :: Coord -> [(Coord, M.Map Coord Int)]
    findEdgesFrom startNode = concatMap (explorePath startNode 1 (S.singleton startNode)) validNeighbors
      where
        validNeighbors = filter (`M.member` grid) (getAllNeighbors startNode)

    -- Explore from a junction's neighbor until another junction is hit
    explorePath :: Coord -> Int -> Visited -> Coord -> [(Coord, M.Map Coord Int)]
    explorePath origin step visited current
        | S.member current junctions = -- Reached another junction (or the origin itself, ignore)
            if current /= origin
            then [(origin, M.singleton current step), (current, M.singleton origin step)] -- Add edge in both directions
            else []
        | otherwise =
            let visited' = S.insert current visited
                -- Find the single next step (must be exactly one in a corridor)
                nextMoves = filter (\n -> M.member n grid && S.notMember n visited') (getAllNeighbors current)
            in case nextMoves of
                 [next] -> explorePath origin (step + 1) visited' next -- Continue down the corridor
                 _      -> [] -- Dead end or hit a junction implicitly (handled above or error)


-- Depth-first search on the condensed graph to find the longest path.
dfsCondensed :: Graph -> Coord -> Coord -> Visited -> Int -> Maybe Int
dfsCondensed graph current target visited currentLen
    | current == target = Just currentLen
    | otherwise =
        let visited' = S.insert current visited
            -- Get neighbors and edge weights from the condensed graph
            neighborEdges = M.toList $ fromMaybe M.empty (M.lookup current graph)
            -- Explore valid neighbors (not already visited in this path)
            paths = mapMaybe (\(neighbor, weight) ->
                                if S.notMember neighbor visited'
                                then dfsCondensed graph neighbor target visited' (currentLen + weight)
                                else Nothing
                             ) neighborEdges
        in if null paths
           then Nothing -- No path found from here
           else Just $ maximum paths

solvePart2 :: Grid -> Coord -> Coord -> Int
solvePart2 grid start end =
    let junctions = findJunctions grid start end
        condensedGraph = buildCondensedGraph grid junctions
    in fromMaybe 0 $ dfsCondensed condensedGraph start end S.empty 0

-- --- Main Execution ---

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (grid, _rows, _cols, start, end) = parseInput input

    -- Solve Part 1
    let result1 = solvePart1 grid start end
    putStrLn $ "Part 1: " ++ show result1

    -- Solve Part 2
    let result2 = solvePart2 grid start end
    putStrLn $ "Part 2: " ++ show result2
