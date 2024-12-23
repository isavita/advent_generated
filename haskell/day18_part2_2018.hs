
import Data.List
import qualified Data.Map as Map

type Grid = [[Char]]

main :: IO ()
main = do
  grid <- readFile "input.txt" >>= return . lines
  let (cycleStart, cycleLength, seenStates) = findCycle grid Map.empty 0
  let remainingMinutes = (1000000000 - cycleStart) `mod` cycleLength
  let finalGrid = iterate transform grid !! (cycleStart + remainingMinutes)
  let (wooded, lumberyards) = countResources finalGrid
  print (wooded * lumberyards)

findCycle :: Grid -> Map.Map String Int -> Int -> (Int, Int, Map.Map String Int)
findCycle grid seenStates minute =
  case Map.lookup (gridToString grid) seenStates of
    Just seenMinute -> (seenMinute, minute - seenMinute, seenStates)
    Nothing -> findCycle (transform grid) (Map.insert (gridToString grid) minute seenStates) (minute + 1)

transform :: Grid -> Grid
transform grid =
  [[nextAcreState grid i j | j <- [0 .. length (head grid) - 1]] | i <- [0 .. length grid - 1]]

nextAcreState :: Grid -> Int -> Int -> Char
nextAcreState grid i j =
  case grid !! i !! j of
    '.' -> if countAdjacent grid i j '|' >= 3 then '|' else '.'
    '|' -> if countAdjacent grid i j '#' >= 3 then '#' else '|'
    '#' -> if countAdjacent grid i j '#' >= 1 && countAdjacent grid i j '|' >= 1 then '#' else '.'
    _ -> grid !! i !! j

countAdjacent :: Grid -> Int -> Int -> Char -> Int
countAdjacent grid i j acreType =
  length $ filter (== acreType) [grid !! x !! y | x <- [max 0 (i - 1) .. min (length grid - 1) (i + 1)],
                                                  y <- [max 0 (j - 1) .. min (length (head grid) - 1) (j + 1)],
                                                  (x, y) /= (i, j)]

countResources :: Grid -> (Int, Int)
countResources grid = (length $ filter (== '|') (concat grid), length $ filter (== '#') (concat grid))

gridToString :: Grid -> String
gridToString = unlines
