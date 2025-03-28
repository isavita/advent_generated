
{-# LANGUAGE BangPatterns #-}

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|), (:|>)), (|>))
import Data.Maybe (mapMaybe)
import GHC.Real (lcm, gcd) -- Use GHC's built-in gcd and lcm
import System.IO (readFile)
import Control.Monad (guard)

-- Type definitions
type Pos = (Int, Int) -- (Row, Col), 0-based index
type Blizzard = (Pos, Char) -- Initial position and direction
type Grid = A.Array Pos Char -- The static map (walls, initial empty spots)
type BlizzardPositions = S.Set Pos -- Set of positions occupied by blizzards at a specific time
type BlizzardCache = A.Array Int BlizzardPositions -- Cache: time `mod` period -> BlizzardPositions
type State = (Int, Int, Int) -- (Time, Row, Col)
type VisitedState = (Int, Int, Int) -- (Time `mod` Period, Row, Col)
type Visited = S.Set VisitedState -- Set of visited states to avoid cycles and redundant work

-- Parses input string into Grid, Blizzards list, Start pos, End pos
parseInput :: String -> (Grid, [Blizzard], Pos, Pos)
parseInput input = (grid, blizzards, startPos, endPos)
  where
    ls = lines input
    numRows = length ls
    numCols = length (head ls) -- Assume all lines have the same length
    bounds = ((0, 0), (numRows - 1, numCols - 1))

    -- Create an array representing the grid
    grid = A.listArray bounds (concat ls)

    -- Find the start position (the only '.' in the first row)
    startPos = head [(0, c) | c <- [0 .. numCols - 1], grid A.! (0, c) == '.']
    -- Find the end position (the only '.' in the last row)
    endPos = head [(numRows - 1, c) | c <- [0 .. numCols - 1], grid A.! (numRows - 1, c) == '.']

    -- Extract initial blizzard positions and directions
    blizzards = mapMaybe getBlizzard (A.assocs grid)
      where
        getBlizzard :: (Pos, Char) -> Maybe Blizzard
        getBlizzard (pos, char)
          | char `elem` ['>', '<', '^', 'v'] = Just (pos, char)
          | otherwise = Nothing

-- Calculates the position of a single blizzard at a given time t
-- Blizzards wrap around the inner grid (excluding the boundary walls)
calcBlizzardPos :: Int -> Int -> Int -> Blizzard -> Pos
calcBlizzardPos !height !width !time ((!r0, !c0), !dir) =
    -- height = number of rows inside the walls (numRows - 2)
    -- width = number of columns inside the walls (numCols - 2)
    -- Grid indices are 0-based. Inner grid rows are 1 to height, cols 1 to width.
    case dir of
        '>' -> (r0, 1 + (c0 - 1 + time) `mod` width)
        '<' -> (r0, 1 + (c0 - 1 - time) `mod` width)
        'v' -> (1 + (r0 - 1 + time) `mod` height, c0)
        '^' -> (1 + (r0 - 1 - time) `mod` height, c0)
        _   -> error "Invalid blizzard direction" -- Should not happen based on parsing

-- Builds a cache of blizzard positions for each time step within a full cycle (LCM of height and width)
buildBlizzardCache :: Int -> Int -> Int -> [Blizzard] -> BlizzardCache
buildBlizzardCache !height !width !period !blizzards =
    A.listArray (0, period - 1) blizzardSetsAtTimeT
  where
    blizzardSetsAtTimeT = [ S.fromList [ calcBlizzardPos height width t b | b <- blizzards ]
                          | t <- [0 .. period - 1] ]

-- Performs Breadth-First Search to find the shortest time to reach the end
findShortestPath :: Grid -> BlizzardCache -> Int -> Pos -> Pos -> Int
findShortestPath grid cache period start end = bfs (Seq.singleton (0, fst start, snd start)) (S.singleton (0, fst start, snd start))
  where
    -- Get grid boundaries
    !bounds@((!minR, !minC), (!maxR, !maxC)) = A.bounds grid

    bfs :: Seq.Seq State -> Visited -> Int
    bfs Seq.Empty _ = error "No path found - BFS queue became empty"
    bfs ((!t, !r, !c) :<| queue) visited
        -- Goal reached! Return the time.
      | (r, c) == end = t
        -- Continue BFS
      | otherwise =
          let
              !nextTime = t + 1
              !timeMod = nextTime `mod` period
              -- Get the set of blocked positions at the *next* minute
              !blockedPositions = cache A.! timeMod

              -- Potential next positions: current position (wait) + neighbors (up, down, left, right)
              !candidates = (r, c) : do -- Using list monad for conciseness
                  (dr, dc) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
                  let !nr = r + dr
                      !nc = c + dc
                  return (nr, nc)

              -- Filter candidates to find valid, non-blocked next steps
              !validMoves = mapMaybe (checkMove blockedPositions) candidates

              -- Check if a candidate move is valid
              checkMove :: BlizzardPositions -> Pos -> Maybe Pos
              checkMove blocked pos@(!nr, !nc) = do
                  -- 1. Check grid bounds (is it within the map?)
                  guard (nr >= minR && nr <= maxR && nc >= minC && nc <= maxC)
                  -- 2. Check for walls (is it not a '#'?)
                  guard (grid A.! pos /= '#')
                  -- 3. Check for blizzards (is it not occupied by a blizzard at nextTime?)
                  guard (not $ S.member pos blocked)
                  -- If all checks pass, it's a valid position
                  return pos

              -- Process valid moves: add new, unvisited states to queue and visited set
              foldStep (!q, !v) pos@(!nr, !nc) =
                  let !visitedKey = (timeMod, nr, nc)
                  in if S.member visitedKey v
                     then (q, v) -- Already visited this state (pos at this time_mod_period)
                     else (q |> (nextTime, nr, nc), S.insert visitedKey v) -- Add to queue and visited

              (!newQueue, !newVisited) = foldl foldStep (queue, visited) validMoves

          in bfs newQueue newVisited


-- Main entry point
main :: IO ()
main = do
    input <- readFile "input.txt"
    let (grid, blizzards, startPos, endPos) = parseInput input
        (_, (!maxR, !maxC)) = A.bounds grid

        -- Calculate inner grid dimensions for blizzard wrapping
        !height = maxR - 1 -- Number of rows available for blizzards (total rows - 2 walls)
        !width = maxC - 1  -- Number of columns available for blizzards (total columns - 2 walls)

        -- Calculate the blizzard pattern cycle length
        !period = lcm height width

        -- Pre-compute blizzard positions for the entire cycle
        !blizzardCache = buildBlizzardCache height width period blizzards

        -- Run BFS to find the shortest time
        !result = findShortestPath grid blizzardCache period startPos endPos

    -- Print the result
    print result
