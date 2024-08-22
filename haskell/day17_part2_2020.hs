import qualified Data.Set as Set
import System.IO

type Point = (Int, Int, Int, Int)
type Grid = Set.Set Point

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let initialGrid = parseInput contents
    let finalGrid = iterate simulateCycle initialGrid !! 6
    print $ Set.size finalGrid

parseInput :: String -> Grid
parseInput input = Set.fromList [(x, y, 0, 0) | (y, line) <- zip [0..] (lines input), (x, char) <- zip [0..] line, char == '#']

simulateCycle :: Grid -> Grid
simulateCycle grid = Set.fromList [point | point <- allPoints, let activeNeighbors = countActiveNeighbors grid point, (point `Set.member` grid && activeNeighbors == 2) || activeNeighbors == 3]
    where
        allPoints = [(x+dx, y+dy, z+dz, w+dw) | (x, y, z, w) <- Set.toList grid, dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw <- [-1..1]]
        countActiveNeighbors g (x, y, z, w) = length [(nx, ny, nz, nw) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw <- [-1..1], let (nx, ny, nz, nw) = (x+dx, y+dy, z+dz, w+dw), (nx, ny, nz, nw) /= (x, y, z, w), (nx, ny, nz, nw) `Set.member` g]