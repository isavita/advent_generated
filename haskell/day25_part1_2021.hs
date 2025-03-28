
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (foldl')
import System.IO (readFile)

-- Type aliases for clarity
type Position = (Int, Int) -- (row, col)
type Grid = Map Position Char
type GridState = (Int, Int, Grid) -- (height, width, grid data)

-- Parses the input string into the initial grid state.
parseInput :: String -> GridState
parseInput input =
    let rows = lines input
        height = length rows
        width = if height > 0 then length (head rows) else 0
        gridData = Map.fromList
            [ ((r, c), char)
            | (r, row) <- zip [0..] rows
            , (c, char) <- zip [0..] row
            ]
    in (height, width, gridData)

-- Calculates the target position for a cucumber based on its type, handling wrapping.
calculateTarget :: Int -> Int -> Position -> Char -> Position
calculateTarget height width (r, c) cucumberType
    | cucumberType == '>' = (r, (c + 1) `mod` width)
    | cucumberType == 'v' = ((r + 1) `mod` height, c)
    | otherwise = error "Invalid cucumber type in calculateTarget" -- Should not happen

-- Finds all cucumbers of a given type that can move in the current grid.
-- Returns a list of (current_position, target_position) pairs.
findMovingCucumbers :: Int -> Int -> Char -> Grid -> [(Position, Position)]
findMovingCucumbers height width cucumberType grid =
    Map.foldrWithKey checkMove [] grid
  where
    checkMove pos cell acc
        | cell == cucumberType =
            let targetPos = calculateTarget height width pos cucumberType
            -- Check if the target position exists and is empty
            in case Map.lookup targetPos grid of
                 Just '.' -> (pos, targetPos) : acc -- Add (current, target) pair if movable
                 _        -> acc                   -- Otherwise, skip
        | otherwise = acc -- Skip if not the right type of cucumber

-- Performs one phase of movement (either east or south).
-- Takes the dimensions, cucumber type, and current grid.
-- Returns the updated grid after this phase.
moveHerd :: Int -> Int -> Char -> Grid -> Grid
moveHerd height width cucumberType grid =
    let movingPairs = findMovingCucumbers height width cucumberType grid
        -- If no cucumbers of this type move, return the original grid
        -- otherwise apply the moves.
    in if null movingPairs
        then grid
        else foldl' applyMove grid movingPairs
  where
    -- Updates the grid for a single cucumber move:
    -- empties the old position and places the cucumber in the new position.
    applyMove :: Grid -> (Position, Position) -> Grid
    applyMove currentGrid (oldPos, newPos) =
        Map.insert newPos cucumberType $ Map.insert oldPos '.' currentGrid

-- Performs one full step of the simulation: first east-movers, then south-movers.
step :: GridState -> GridState
step (height, width, grid) =
    let gridAfterEast = moveHerd height width '>' grid
        gridAfterSouth = moveHerd height width 'v' gridAfterEast
    in (height, width, gridAfterSouth)

-- Simulates the sea cucumber movement until they stop.
-- Takes the current step count and the grid state.
-- Returns the final step count when movement stops.
simulate :: Int -> GridState -> Int
simulate count currentState@(_, _, currentGrid) =
    let nextState@(_, _, nextGrid) = step currentState
    in if currentGrid == nextGrid -- Check if the grid has changed
        then count                  -- If not, return the current step count
        else simulate (count + 1) nextState -- Otherwise, continue simulation

-- Main entry point
main :: IO ()
main = do
    -- Read input from the file
    input <- readFile "input.txt"

    -- Parse the initial state
    let initialState = parseInput input

    -- Run the simulation starting from step 1
    let finalStepCount = simulate 1 initialState

    -- Print the result to standard output
    print finalStepCount
