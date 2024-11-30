
import Data.List (transpose)

type Grid = [[Char]]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let grid = lines contents
      startPos = findStart grid
      reachable = reachablePlots grid startPos 64
  print $ length reachable

findStart :: Grid -> (Int, Int)
findStart grid = head [(x, y) | (x, row) <- zip [0..] grid, (y, c) <- zip [0..] row, c == 'S']

reachablePlots :: Grid -> (Int, Int) -> Int -> [(Int, Int)]
reachablePlots grid startPos steps = reachableHelper grid [startPos] 0 steps

reachableHelper :: Grid -> [(Int, Int)] -> Int -> Int -> [(Int, Int)]
reachableHelper grid currentPositions currentStep targetSteps
  | currentStep == targetSteps = nub currentPositions
  | otherwise = 
    let nextPositions = concatMap (possibleNextPositions grid) currentPositions
        uniqueNextPositions = nub nextPositions
    in reachableHelper grid uniqueNextPositions (currentStep + 1) targetSteps


possibleNextPositions :: Grid -> (Int, Int) -> [(Int, Int)]
possibleNextPositions grid (x, y) = filter (\(x',y') -> isValidMove grid (x',y')) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

isValidMove :: Grid -> (Int, Int) -> Bool
isValidMove grid (x, y) = 
  let rows = length grid
      cols = length (head grid)
  in x >= 0 && x < rows && y >= 0 && y < cols && grid !! x !! y /= '#'

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
