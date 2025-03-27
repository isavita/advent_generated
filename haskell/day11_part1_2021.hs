
import Data.Array
import Data.Char (digitToInt)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import Control.Monad (guard)
import System.IO (readFile)

type Grid = Array (Int, Int) Int
type Point = (Int, Int)

neighbors :: Grid -> Point -> [Point]
neighbors grid (r, c) = do
    dr <- [-1, 0, 1]
    dc <- [-1, 0, 1]
    guard (dr /= 0 || dc /= 0)
    let nr = r + dr
    let nc = c + dc
    guard (inRange (bounds grid) (nr, nc))
    return (nr, nc)

doFlashes :: Grid -> Set Point -> [Point] -> (Grid, Set Point)
doFlashes grid flashed [] = (grid, flashed)
doFlashes grid flashed (p:toFlash)
    | Set.member p flashed = doFlashes grid flashed toFlash
    | otherwise =
        let
            newFlashed = Set.insert p flashed
            processNeighbor (g, newly) nCoord =
                let currentEnergy = g ! nCoord
                    newEnergy = currentEnergy + 1
                    nextG = g // [(nCoord, newEnergy)]
                in if newEnergy == 10
                      then (nextG, nCoord : newly)
                      else (nextG, newly)
            (updatedGrid, newlyFlashing) = foldl' processNeighbor (grid, []) (neighbors grid p)
        in doFlashes updatedGrid newFlashed (toFlash ++ newlyFlashing)

simulateStep :: Grid -> (Int, Grid)
simulateStep grid =
    let
        incGrid = fmap (+1) grid
        initialFlashers = [idx | idx <- indices grid, incGrid ! idx > 9]
        (flashedGrid, flashedSet) = doFlashes incGrid Set.empty initialFlashers
        resetGrid = flashedGrid // [(p, 0) | p <- Set.toList flashedSet]
    in (Set.size flashedSet, resetGrid)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let rows = lines contents
        numRows = length rows
        numCols = if numRows > 0 then length (head rows) else 0
        gridData = [((r, c), digitToInt char)
                   | (r, row) <- zip [0..] rows
                   , (c, char) <- zip [0..] row]
    let initialGrid = array ((0, 0), (numRows - 1, numCols - 1)) gridData :: Grid
    let runStep (flashesAcc, currentGrid) _ =
            let (stepFlashes, nextGrid) = simulateStep currentGrid
            in seq nextGrid (flashesAcc + stepFlashes, nextGrid)
    let (totalFlashes, _) = foldl' runStep (0, initialGrid) [1..100]
    print totalFlashes
