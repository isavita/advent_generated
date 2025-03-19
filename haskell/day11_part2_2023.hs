
import Data.List (transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Coord = (Int, Int)
type Grid = M.Map Coord Char

parseGrid :: [String] -> Grid
parseGrid lines =
  M.fromList $
    [ ((x, y), c)
      | (y, row) <- zip [0 ..] lines,
        (x, c) <- zip [0 ..] row,
        c /= '.'
    ]

emptyRows :: [String] -> [Int]
emptyRows grid = [y | (y, row) <- zip [0 ..] grid, all (== '.') row]

emptyCols :: [String] -> [Int]
emptyCols grid = emptyRows (transpose grid)

expand :: Int -> [Int] -> Int -> [Int]
expand factor emptyIndices bound =
  scanl (\acc x -> if x `elem` emptyIndices then acc + factor else acc + 1) (-1) [0 .. bound - 1]

solve :: Int -> [String] -> Int
solve factor gridLines =
  let grid = parseGrid gridLines
      height = length gridLines
      width = length (head gridLines)
      emptyRowsIndices = emptyRows gridLines
      emptyColsIndices = emptyCols gridLines
      dx = expand factor emptyColsIndices width
      dy = expand factor emptyRowsIndices height
      expandedCoords =
        [ ((dx !! x, dy !! y), c)
          | ((x, y), c) <- M.toList grid
        ]
      coords = map fst expandedCoords
      pairs = [(c1, c2) | c1 <- coords, c2 <- coords, c1 < c2]
   in sum [abs (x1 - x2) + abs (y1 - y2) | ((x1, y1), (x2, y2)) <- pairs]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let gridLines = lines input
  print (solve 1000000 gridLines)
