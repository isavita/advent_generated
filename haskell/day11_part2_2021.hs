
import Data.Array.Unboxed
import Data.Char (digitToInt)
import qualified Data.Set as Set
import Data.Set (Set)

type Coord = (Int, Int)
type Grid = UArray Coord Int
type GridState = (Grid, Set Coord) -- Grid and the set of octopuses that have flashed in the current step

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
        rows = length inputLines
        cols = length (head inputLines)
        bnds = ((0, 0), (rows - 1, cols - 1))
        initialGrid = array bnds
            [((r, c), digitToInt char)
            | (r, line) <- zip [0..] inputLines
            , (c, char) <- zip [0..] line
            ]
    print $ findSyncStep 0 initialGrid (rows * cols)

findSyncStep :: Int -> Grid -> Int -> Int
findSyncStep count grid totalOctopuses =
    let (flashes, nextGrid) = simulateStep grid
    in if flashes == totalOctopuses
       then count + 1
       else findSyncStep (count + 1) nextGrid totalOctopuses

simulateStep :: Grid -> (Int, Grid)
simulateStep grid =
    let bnds = bounds grid
        -- 1. Increment all energy levels
        incGrid = amap (+1) grid

        -- 2. Find initial flashers
        initialFlashers = [coord | coord <- range bnds, incGrid ! coord > 9]

        -- 3. Perform cascading flashes
        (finalGridAfterFlash, flashedSet) = processFlashes bnds (incGrid, Set.empty) initialFlashers

        -- 4. Reset flashed octopuses to 0
        resetGrid = finalGridAfterFlash // [(coord, 0) | coord <- Set.toList flashedSet]

    in (Set.size flashedSet, resetGrid)


processFlashes :: (Coord, Coord) -> GridState -> [Coord] -> GridState
processFlashes _ gridState [] = gridState
processFlashes bnds (currentGrid, flashed) (coord:queue) =
    if Set.member coord flashed || currentGrid ! coord <= 9
    then processFlashes bnds (currentGrid, flashed) queue -- Already flashed or not enough energy
    else
        let newFlashed = Set.insert coord flashed
            adjCoords = neighbors bnds coord
            -- Increment neighbors and collect updates
            updates = [(nCoord, currentGrid ! nCoord + 1) | nCoord <- adjCoords]
            gridAfterInc = currentGrid // updates
            -- Find new flashers among neighbors
            newFlashers = [nCoord | (nCoord, val) <- updates, val > 9, Set.notMember nCoord newFlashed]
        in processFlashes bnds (gridAfterInc, newFlashed) (queue ++ newFlashers)


neighbors :: (Coord, Coord) -> Coord -> [Coord]
neighbors bnds (r, c) =
    [ (nr, nc)
    | dr <- [-1, 0, 1]
    , dc <- [-1, 0, 1]
    , (dr, dc) /= (0, 0)
    , let nr = r + dr
    , let nc = c + dc
    , inRange bnds (nr, nc)
    ]

