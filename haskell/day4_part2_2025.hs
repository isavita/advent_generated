
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (foldl')
import Data.Char (isSpace)

type Pos = (Int, Int)
type Grid = S.Set Pos

-- Parse input into a set of positions with paper rolls
parseInput :: String -> Grid
parseInput input = S.fromList
  [ (x, y)
  | (y, line) <- zip [0..] (lines input)
  , (x, char) <- zip [0..] line
  , char == '@'
  ]

-- Get all 8 adjacent positions
neighbors :: Pos -> [Pos]
neighbors (x, y) =
  [ (x+dx, y+dy)
  | dx <- [-1..1]
  , dy <- [-1..1]
  , (dx, dy) /= (0, 0)
  ]

-- Count adjacent paper rolls for a position
countAdjacent :: Grid -> Pos -> Int
countAdjacent grid pos = length $ filter (`S.member` grid) (neighbors pos)

-- Find all positions that can be accessed (have < 4 adjacent rolls)
findAccessible :: Grid -> S.Set Pos
findAccessible grid = S.filter isAccessible grid
  where
    isAccessible pos = countAdjacent grid pos < 4

-- Remove all accessible positions and return new grid and count removed
removeAccessible :: Grid -> (Grid, Int)
removeAccessible grid = (S.difference grid accessible, S.size accessible)
  where
    accessible = findAccessible grid

-- Repeatedly remove accessible until none left
removeAll :: Grid -> Int
removeAll = go 0
  where
    go !count grid
      | S.null accessible = count
      | otherwise = go (count + removed) newGrid
      where
        accessible = findAccessible grid
        (newGrid, removed) = removeAccessible grid

main :: IO ()
main = do
  content <- readFile "input.txt"
  let grid = parseInput content
  
  -- Part 1: Count initially accessible
  let part1 = S.size $ findAccessible grid
  print part1
  
  -- Part 2: Count total removable
  print $ removeAll grid
