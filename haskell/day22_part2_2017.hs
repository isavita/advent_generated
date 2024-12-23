
import qualified Data.Map as M
import Data.List (foldl')

type Pos = (Int, Int)
data State = Clean | Weakened | Infected | Flagged deriving (Eq, Ord, Show, Enum)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let grid = parseGrid $ lines input
      (startX, startY) = (length (head (lines input)) `div` 2, length (lines input) `div` 2)
      result = solve grid (startX, startY) 0 0
  print result

parseGrid :: [String] -> M.Map Pos State
parseGrid ls = foldl' (\acc (y, row) -> foldl' (\acc' (x, c) -> if c == '#' then M.insert (x, y) Infected acc' else acc') acc (zip [0..] row)) M.empty (zip [0..] ls)

solve :: M.Map Pos State -> Pos -> Int -> Int -> Int
solve grid (x, y) dir infectedCount =
  if infectedCount == 10000000
  then 0
  else
    let
      pos = (x, y)
      currentState = M.findWithDefault Clean pos grid
      (newDir, newState, newInfected) = updateState currentState dir infectedCount
      (dx, dy) = [(0, -1), (1, 0), (0, 1), (-1, 0)] !! newDir
      newGrid = M.insert pos newState grid
    in newInfected + solve newGrid (x + dx, y + dy) newDir (infectedCount + 1)

updateState :: State -> Int -> Int -> (Int, State, Int)
updateState Clean dir infectedCount = ((dir - 1 + 4) `mod` 4, Weakened, 0)
updateState Weakened dir infectedCount = (dir, Infected, 1)
updateState Infected dir infectedCount = ((dir + 1) `mod` 4, Flagged, 0)
updateState Flagged dir infectedCount = ((dir + 2) `mod` 4, Clean, 0)
